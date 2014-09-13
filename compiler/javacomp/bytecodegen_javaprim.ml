(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java compiler is free software; you can redistribute it and/or modify
 * it under the terms of the Q Public License as published by
 * Trolltech (with a change to choice of law).
 *
 * OCaml-Java compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Q Public License for more details.
 *
 * You should have received a copy of the Q Public License
 * along with this program.  If not, see
 * <http://opensource.org/licenses/QPL-1.0>.
 *)

open Jlambda
open Bytecodeutils
open BaristaLibrary
open Bytecodegen_misc


let node x = Instrtree.node x

let leaf x = Instrtree.leaf x

let size x = Instrtree.size x

let with_size x = x, size x

let compile_type (jt : java_type) =
  let get_type t =
    Instruction.GETSTATIC (make_class ("java.lang." ^ t),
                           make_field "TYPE",
                           `Class class_Class) in
  [match jt with
  | `Boolean          -> get_type "Boolean"
  | `Byte             -> get_type "Byte"
  | `Char             -> get_type "Character"
  | `Double           -> get_type "Double"
  | `Float            -> get_type "Float"
  | `Int              -> get_type "Integer"
  | `Long             -> get_type "Long"
  | `Short            -> get_type "Short"
  | `Void             -> get_type "Void"
  | `Class cn         -> Instruction.LDC_W (`Class_or_interface (make_class cn))
  | (`Array _) as arr -> Instruction.LDC_W (`Array_type (unconvert_array_type arr))]
  |> leaf

let compile_mapping l =
  let adds =
    List.map
      (fun { jppb_interface; jppb_method; jppb_parameters; jppb_ocaml_name } ->
        let params =
          List.mapi
            (fun i p ->
              node
                [ leaf [ Instruction.DUP ] ;
                  push_int32 (Int32.of_int i) ;
                  compile_type (p :> java_type) ;
                  leaf [ Instruction.AASTORE ] ])
            jppb_parameters in
        node
          [ leaf [ Instruction.DUP ;
                   Instruction.LDC_W (`Class_or_interface (make_class jppb_interface)) ;
                   Instruction.LDC_W (`String (UTF8.of_string jppb_method)) ] ;
            push_int32 (Int32.of_int (List.length jppb_parameters)) ;
            leaf [ Instruction.ANEWARRAY (`Class_or_interface class_Class) ] ;
            node params ;
            leaf [ Instruction.LDC_W (`String (UTF8.of_string jppb_ocaml_name)) ] ;
            meth_add ])
      l in
  node
    [ leaf [ Instruction.NEW class_MethodMapping ;
             Instruction.DUP ] ;
      cstr_MethodMapping ;
      node adds ]

let untag =
  Instrtree.leaf
    [ Instruction.ICONST_1 ;
      Instruction.LSHR ;
      Instruction.L2I ]

let tag =
  Instrtree.leaf
    [ Instruction.I2L ;
      Instruction.ICONST_1 ;
      Instruction.LSHL ;
      Instruction.LCONST_1 ;
      Instruction.LOR ]

let convert_to_java = function
  | `Boolean -> untag
  | `Byte    -> untag
  | `Char    -> untag
  | `Double  -> Instrtree.leaf []
  | `Float   -> Instrtree.leaf [ Instruction.D2F ]
  | `Int     -> Instrtree.leaf []
  | `Long    -> Instrtree.leaf []
  | `Short   -> untag
  | `Class _ -> Instrtree.leaf [] (* cast is done by explicit conversion *)
  | `Array _ -> Instrtree.leaf [] (* cast is done by explicit conversion *)

let convert_from_java = function
  | `Boolean -> tag
  | `Byte    -> tag
  | `Char    -> tag
  | `Double  -> Instrtree.leaf []
  | `Float   -> Instrtree.leaf [ Instruction.F2D ]
  | `Int     -> Instrtree.leaf []
  | `Long    -> Instrtree.leaf []
  | `Short   -> tag
  | `Class _ -> Instrtree.leaf [] (* cast is done by explicit conversion *)
  | `Array _ -> Instrtree.leaf [] (* cast is done by explicit conversion *)
  | `Void    -> field_Value_UNIT

let extract_primitive_array : Descriptor.array_type -> Descriptor.java_type option = function
  | `Array `Boolean -> Some `Boolean
  | `Array `Byte    -> Some `Byte
  | `Array `Char    -> Some `Char
  | `Array `Double  -> Some `Double
  | `Array `Float   -> Some `Float
  | `Array `Int     -> Some `Int
  | `Array `Long    -> Some `Long
  | `Array `Short   -> Some `Short
  | _               -> None

let load_and_base : Descriptor.array_type -> Instruction.t * Descriptor.non_void_java_type = function
  | `Array `Boolean -> Instruction.BALOAD, `Boolean
  | `Array `Byte    -> Instruction.BALOAD, `Byte
  | `Array `Char    -> Instruction.CALOAD, `Char
  | `Array `Double  -> Instruction.DALOAD, `Double
  | `Array `Float   -> Instruction.FALOAD, `Float
  | `Array `Int     -> Instruction.IALOAD, `Int
  | `Array `Long    -> Instruction.LALOAD, `Long
  | `Array `Short   -> Instruction.SALOAD, `Short
  | `Array ((`Class _ | `Array _) as b) -> Instruction.AALOAD, b

let store_and_base : Descriptor.array_type -> Instruction.t * Descriptor.non_void_java_type = function
  | `Array `Boolean -> Instruction.BASTORE, `Boolean
  | `Array `Byte    -> Instruction.BASTORE, `Byte
  | `Array `Char    -> Instruction.CASTORE, `Char
  | `Array `Double  -> Instruction.DASTORE, `Double
  | `Array `Float   -> Instruction.FASTORE, `Float
  | `Array `Int     -> Instruction.IASTORE, `Int
  | `Array `Long    -> Instruction.LASTORE, `Long
  | `Array `Short   -> Instruction.SASTORE, `Short
  | `Array ((`Class _ | `Array _) as b) -> Instruction.AASTORE, b

let rec compile_java_primitive ofs jprim prim_args compile_expression_list compile_expression =
  match jprim with
  | Java_constructor (class_name, types, ellipsis) ->
      let class_name = make_class class_name in
      let params = unconvert_java_types types in
      let discard_unit, prim_args, ofs =
        match types, prim_args with
        | [], [arg] ->
            let instrs =
              node
                [ compile_expression false ofs arg ;
                  leaf [ Instruction.POP ] ] in
            instrs, [], ofs + (size instrs)
        | [], _ -> assert false
        | _ -> Instrtree.leaf [], prim_args, ofs in
      node
        [ discard_unit ;
          leaf [ Instruction.NEW class_name ;
                 Instruction.DUP ] ;
          compile_expression_list_java ~ellipsis (ofs + 3 + 1) prim_args params compile_expression;
          leaf [ Instruction.INVOKESPECIAL (class_name,
                                            make_method "<init>",
                                            (params, `Void)) ] ]
  | Java_array (typ, dims) ->
      compile_array ofs typ dims prim_args compile_expression
  | Java_array_get typ ->
      let typ = unconvert_array_type typ in
      let instr, base = load_and_base typ in
      let params = [(typ :> Descriptor.non_void_java_type); `Int] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ instr ] ;
          convert_from_java base ]
  | Java_array_set typ ->
      let typ = unconvert_array_type typ in
      let instr, base = store_and_base typ in
      let params = [(typ :> Descriptor.non_void_java_type); `Int; base] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ instr ] ;
          field_Value_UNIT ]
  | Java_array_length typ ->
      let typ = unconvert_array_type typ in
      let params = [(typ :> Descriptor.non_void_java_type)] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ Instruction.ARRAYLENGTH ] ]
  | Java_array_to_object typ ->
      let typ = unconvert_array_type typ in
      let params = [(typ :> Descriptor.non_void_java_type)] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ Instruction.CHECKCAST (`Class_or_interface class_Object) ] ]
  | Java_array_of_object typ ->
      let typ = unconvert_array_type typ in
      let params = [ `Class class_Object ] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ Instruction.CHECKCAST (`Array_type typ) ] ]
  | Java_method (class_name, method_name, call_kind, types, ellipsis, typ) ->
      let class_name = make_class class_name in
      let method_name = make_method method_name in
      let params = unconvert_java_types types in
      let return = unconvert_java_type typ in
      let desc = params, return in
      let discard_unit, prim_args, ofs =
        match call_kind, types, prim_args with
        | Static_call, [], [arg] ->
            let instrs =
              node
                [ compile_expression false ofs arg ;
                  leaf [ Instruction.POP ] ] in
            instrs, [], ofs + (size instrs)
        | Static_call, [], _ -> assert false
        | _ -> Instrtree.leaf [], prim_args, ofs in
      let call, params = 
        match call_kind with
        | Static_call ->
            Instruction.INVOKESTATIC (class_name, method_name, desc),
            params
        | Interface_call ->
            Instruction.INVOKEINTERFACE ((class_name, method_name, desc)),
            (`Class class_name) :: params
        | Virtual_call ->
            Instruction.INVOKEVIRTUAL (`Class_or_interface class_name, method_name, desc),
            (`Class class_name) :: params in
    node
        [ discard_unit ;
          compile_expression_list_java ~ellipsis ofs prim_args params compile_expression ;
          leaf [ call ] ;
          convert_from_java return ]
  | Java_get (class_name, field_name, field_kind, typ) ->
      let class_name = make_class class_name in
      let field_name = make_field field_name in
      let typ = unconvert_java_type_no_void typ in
      begin match field_kind with
      | Static_field ->
          node
            [ compile_expression_list ofs prim_args ;
              leaf [ Instruction.POP ;
                     Instruction.GETSTATIC (class_name, field_name, typ) ] ;
              convert_from_java typ ]
      | Instance_field ->
          let params = [`Class class_name] in
          node
            [ compile_expression_list_java ofs prim_args params compile_expression ;
              leaf [ Instruction.GETFIELD (class_name, field_name, typ) ] ;
              convert_from_java typ ]
      end
  | Java_set (class_name, field_name, field_kind, typ) ->
      let class_name = make_class class_name in
      let field_name = make_field field_name in
      let typ = unconvert_java_type_no_void typ in
      begin match field_kind with
      | Static_field ->
          let params = [typ] in
          node
            [ compile_expression_list_java ofs prim_args params compile_expression ;
              leaf [ Instruction.PUTSTATIC (class_name, field_name, typ) ] ;
              field_Value_UNIT ]
      | Instance_field ->
          let params = [`Class class_name; typ] in
          node
            [ compile_expression_list_java ofs prim_args params compile_expression ;
              leaf [ Instruction.PUTFIELD (class_name, field_name, typ) ] ;
              field_Value_UNIT ]
      end
  | Java_get_null ->
      node
        [ compile_expression_list ofs prim_args ;
          leaf [ Instruction.POP ;
                 Instruction.ACONST_NULL ] ]
  | Java_is_null | Java_is_not_null->
      let params = [ `Class class_Object ] in
      let if_size = 3 in
      let goto_size = 3 in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ (if jprim = Java_is_null then
                    Instruction.IFNULL (Utils.s2 (if_size + 1 + goto_size))
                  else
                    Instruction.IFNONNULL (Utils.s2 (if_size + 1 + goto_size)));
                 Instruction.LCONST_0 ;
                 Instruction.GOTO (Utils.s2 (goto_size + 1)) ;
                 Instruction.LCONST_1 ] ]
  | Java_equal | Java_not_equal ->
      let params = [ `Class class_Object; `Class class_Object ] in
      let if_size = 3 in
      let goto_size = 3 in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ (if jprim = Java_equal then
                    Instruction.IF_ACMPEQ (Utils.s2 (if_size + 1 + goto_size))
                  else
                    Instruction.IF_ACMPNE (Utils.s2 (if_size + 1 + goto_size))) ;
                 Instruction.LCONST_0 ;
                 Instruction.GOTO (Utils.s2 (goto_size + 1)) ;
                 Instruction.LCONST_1 ] ]
  | Java_instanceof typ | Java_cast typ ->
      let typ =
        match unconvert_java_type_no_void typ with
        | `Class cn       -> `Class_or_interface cn
        | (`Array _) as t -> `Array_type t
        | _ -> assert false in
      let params = [ `Class class_Object ] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          (match jprim with
          | Java_instanceof _ ->
              leaf [ Instruction.INSTANCEOF typ ;
                     Instruction.I2L ]
          | Java_cast _ ->
              leaf [ Instruction.CHECKCAST typ ]
          | _ ->
              assert false) ]
  | Java_class typ ->
      compile_type typ
  | Java_throw ->
      let params = [ `Class (make_class "java.lang.Throwable") ] in
      node
        [ compile_expression_list_java ofs prim_args params compile_expression ;
          leaf [ Instruction.ATHROW ] ]
  | Java_synchronized (_, idx) ->
      begin match prim_args with
      | [ lock; body ] ->
          let goto_size = 3 in
          let lock_instrs, lock_sz = with_size (compile_expression false ofs lock) in
          let store_instrs, store_sz = with_size (astore idx) in
          let load_instrs, load_sz = with_size (aload idx) in
          let body_instrs, body_sz = with_size (compile_expression false (ofs + lock_sz + 1 + store_sz + 1) body) in
          let ofs_body = ofs + lock_sz + 1 + store_sz + 1 in
          let ofs_handler = ofs + lock_sz + 1 + store_sz + 1 + body_sz + load_sz + 1 + goto_size in
          State.add_exception
            (Utils.u2 ofs_body)
            (Utils.u2 (ofs_handler - goto_size))
            (Utils.u2 ofs_handler);
          State.add_exception
            (Utils.u2 ofs_handler)
            (Utils.u2 (ofs_handler + load_sz + 1 + 1))
            (Utils.u2 ofs_handler);
          node
            [ lock_instrs ;
              leaf [ Instruction.DUP ] ;
              store_instrs ;
              leaf [ Instruction.MONITORENTER ] ;
              body_instrs ;
              load_instrs ;
              leaf [ Instruction.MONITOREXIT ] ;
              leaf [ Instruction.GOTO (check_jump (goto_size + load_sz + 1 + 1)) ] ;
              load_instrs ;
              leaf [ Instruction.MONITOREXIT ] ;
              leaf [ Instruction.ATHROW ] ]
      | _ -> assert false
      end
  | Java_proxy { jpp_kind; jpp_interface; jpp_interfaces; jpp_mapping } ->
      let jpp_interfaces =
        jpp_interfaces
        |> List.filter (fun x -> x <> "java.lang.Object")
        |> List.map make_class in
      let set_elements =
        List.fold_left
          (fun (acc_idx, acc_instrs) elem ->
            let instrs = 
              leaf [ Instruction.DUP ;
                     Instruction.LDC_W (`Int acc_idx) ;
                     Instruction.LDC_W (`Class_or_interface elem) ;
                     Instruction.AASTORE ] in
            (Int32.succ acc_idx, instrs :: acc_instrs))
          (0l, [])
          jpp_interfaces
        |> snd in
      let array_instrs, array_sz =
        node
          [ leaf [ Instruction.LDC_W (`Int (Int32.of_int (List.length jpp_interfaces))) ;
                   Instruction.ANEWARRAY (`Class_or_interface class_Class) ] ;
            node set_elements ]
        |> with_size in
      let args_instrs = compile_expression_list (ofs + array_sz + 1) prim_args in
      node
        [ array_instrs ;
          args_instrs ; 
          compile_mapping jpp_mapping ;
          begin match jpp_kind with
          | Custom_class_loader  ->  meth_make_proxy_loader
          | System_class_loader  ->  meth_make_proxy_system
          | Runtime_class_loader ->  meth_make_proxy_runtime
          end ;
          leaf [ Instruction.CHECKCAST (`Class_or_interface (make_class jpp_interface)) ] ]

and compile_array ofs typ { jpad_total; jpad_init } prim_args compile_expression =
  let typ = unconvert_array_type typ in
  begin match extract_primitive_array typ, jpad_total, jpad_init with
  | Some base, 1, _ ->
      node
        [ compile_expression_list_java ofs prim_args [`Int] compile_expression ;
          leaf [ Instruction.NEWARRAY base ] ]
  | None, 1, _ ->
      begin match typ with
      | `Array (`Class cn) ->
          node
            [ compile_expression_list_java ofs prim_args [`Int] compile_expression ;
              leaf [ Instruction.ANEWARRAY (`Class_or_interface cn) ] ]
      | _ -> assert false
      end
  | _, _, 1 ->
      let typ =
        match typ with
        | `Array (`Array x) -> `Array_type (`Array x)
        | _ -> assert false in
      node
        [ compile_expression_list_java ofs prim_args [`Int] compile_expression ;
          leaf [ Instruction.ANEWARRAY typ ] ]
  | _ ->
      let rec ints n = if n <= 0 then [] else `Int :: (ints (pred n)) in
      node
        [ compile_expression_list_java ofs prim_args (ints jpad_init) compile_expression ;
          leaf [ Instruction.MULTIANEWARRAY (`Array_type typ, Utils.u1 jpad_init) ] ]
  end

and compile_expression_list_java ?(ellipsis=false) ofs exprs types compile_expression =
  let len = List.length exprs in
  let _, _, res = 
    List.fold_left2
      (fun (idx, curr_ofs, acc) expr (typ : Descriptor.non_void_java_type) ->
        if ellipsis && (idx = pred len) then begin
          let typ =
            (typ :> Descriptor.java_type)
            |> Descriptor.(filter_non_array Invalid_array_element_type) in
          let store, base = store_and_base typ in
          let typ = convert_array_type typ in
          let arr_len =
            (* Cannot be a Mconst: the only possible values would be
               Const_null or Const_javastring because varagrs elements
               are Java instances. However, we are expecting here an
               array of literal values, and such Const_null/Const_javastring
               values cannot be embedded in a Lambda_const (Lambda.Const_block ...). *)
            match expr with
            | Macroinstr.Mprim (Lambda.Pmakeblock _, l, _)
            | Macroinstr.Mconvert (_, _, Macroinstr.Mprim (Lambda.Pmakeblock _, l, _))
            | Macroinstr.Mprim (Lambda.Pmakearray _, l, _)
            | Macroinstr.Mconvert (_, _, Macroinstr.Mprim (Lambda.Pmakearray _, l, _)) ->
                Int32.of_int (List.length l)
            | _ ->
                raise (Error Varargs_should_be_literal_array) in
          let array_instr, array_size =
            compile_array
              curr_ofs
              typ
              { jpad_total = 1; jpad_init = 1 }
              [ Macroinstr.Mconst
                  (Lambda_const
                     (Lambda.Const_base
                        (Asttypes.Const_int32 arr_len))) ]
              compile_expression
            |> with_size in
          let ofs = ref (curr_ofs + array_size) in
          let init_instrs = ref [] in
          begin match expr with
          | Macroinstr.Mprim (Lambda.Pmakeblock _, l, _)
          | Macroinstr.Mconvert (_, _, Macroinstr.Mprim (Lambda.Pmakeblock _, l, _))
          | Macroinstr.Mprim (Lambda.Pmakearray _, l, _)
          | Macroinstr.Mconvert (_, _, Macroinstr.Mprim (Lambda.Pmakearray _, l, _)) ->
              List.iteri
                (fun idx e ->
                  let i, s =
                    let dst =
                      let open Macroinstr in
                      match convert_java_type_no_void base with
                      | `Boolean   -> Normalized_int
                      | `Byte      -> Normalized_int
                      | `Char      -> Normalized_int
                      | `Double    -> Boxed_value
                      | `Float     -> Boxed_value
                      | `Int       -> Unboxed_int32
                      | `Long      -> Unboxed_int64
                      | `Short     -> Normalized_int
                      | `Class cn  -> Unboxed_instance cn
                      | `Array arr -> Unboxed_java_array (`Array arr) in
                    let e = Macroinstr.(Mconvert (Boxed_value, dst, e)) in
                    node
                      [ leaf [ Instruction.DUP ] ;
                        push_int32 (Int32.of_int idx) ;
                        compile_expression false !ofs e ;
                        (match base with
                        | `Boolean -> leaf [ Instruction.L2I ]
                        | `Byte    -> leaf [ Instruction.L2I ]
                        | `Char    -> leaf [ Instruction.L2I ]
                        | `Double  -> leaf []
                        | `Float   -> leaf [ Instruction.D2F ]
                        | `Int     -> leaf []
                        | `Long    -> leaf []
                        | `Short   -> leaf [ Instruction.L2I ]
                        | `Class _ -> leaf []
                        | `Array _ -> leaf []) ;
                        leaf [ store ] ]
                    |> with_size in
                  ofs := !ofs + s;
                  init_instrs := i :: !init_instrs)
                l;
          | _ ->
              assert false
          end;
          succ idx,
          !ofs,
          (node (array_instr :: (List.rev !init_instrs))) :: acc
        end else begin
          let instrs, size =
            node
              [ compile_expression false curr_ofs expr ;
                convert_to_java typ ]
            |> with_size in
          succ idx, curr_ofs + size, instrs :: acc
        end)
      (0, ofs, [])
      exprs
      types in
  res
  |> List.rev
  |> node
