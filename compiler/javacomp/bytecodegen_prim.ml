(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
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

open Asttypes
open Lambda
open Macroinstr
open Bytecodeutils
open Bytecodegen_misc
open BaristaLibrary


let node x = Instrtree.node x

let leaf x = Instrtree.leaf x

let size x = Instrtree.size x

let with_size x = x, size x

let inline_simple_arithmetic () =
  !Clflags.inline_threshold >= 100

let get_global id =
  if ((State.current_function ()) = "entry")
      && ((State.current_module ()) = (Ident.name id)) then begin
    make_class (Jcompilenv.current_global_class_name ()),
    leaf [ Instruction.ALOAD_0 ]
  end else begin
    try
      let classname = Jcompilenv.class_for_global id in
      let glob_classname = make_class (classname ^ "$Global") in
      let classname = make_class classname in
      glob_classname,
      node
        [ leaf [ Instruction.GETSTATIC (classname,
                                        make_field "GLOBALS",
                                        `Class class_ThreadLocal) ] ;
          meth_get_threadlocal ;
          leaf [ Instruction.CHECKCAST (`Class_or_interface glob_classname) ] ]
    with _ ->
      make_class "dummy_class",
      node
        [ leaf [ Instruction.LDC_W (`String (UTF8.of_string (Ident.name id))) ];
          meth_getGlobal ]
  end

let optimized_get_global id idx =
  (match Jcompilenv.global_approx_no_dep id with
  | Jlambda.Value_tuple app when idx < Array.length app -> true
  | Jlambda.Value_tuple _                               -> false
  | _                                                   ->false)
    &&
  (try
    ignore (Jcompilenv.class_for_global id); true
  with _ ->
    false)

let descriptor_of_approx = function
  | Jlambda.Value_closure _           -> `Class class_Value
  | Jlambda.Value_tuple _             -> `Class class_Value
  | Jlambda.Value_unknown None        -> `Class class_Value
  | Jlambda.Value_unknown (Some repr) ->
      begin match repr with
      | LR_value | LR_string | LR_exn | LR_array _
      | LR_list _ | LR_option _ | LR_lazy _ | LR_unit ->
          `Class class_Value
      | LR_int | LR_char | LR_bool ->
          `Long
      | LR_float ->
          `Double
      | LR_nativeint ->
          `Long
      | LR_int32 ->
          `Int
      | LR_int64 ->
          `Long
      | LR_java_instance cn | LR_java_extends cn ->
          `Class (make_class cn)
      | (LR_java_boolean_array
      | LR_java_byte_array
      | LR_java_char_array
      | LR_java_double_array
      | LR_java_float_array
      | LR_java_int_array
      | LR_java_long_array
      | LR_java_reference_array _
      | LR_java_short_array) as arr ->
          let arr = array_type_of_repr arr in
          (Jlambda.unconvert_array_type arr :> Descriptor.for_field)
      | LR_none ->
          assert false
      end
  | Jlambda.Value_integer _           -> `Long
  | Jlambda.Value_integer32 _         -> `Int
  | Jlambda.Value_integer64 _         -> `Long
  | Jlambda.Value_integernat _        -> `Long
  | Jlambda.Value_constptr _          -> `Long
  | Jlambda.Value_float _             -> `Double
  | Jlambda.Value_java_null None      -> `Class class_Object
  | Jlambda.Value_java_null (Some jt) ->
      begin match jt with
      | `Class cn ->
          `Class (make_class cn)
      | (`Array _) as arr ->
          (Jlambda.unconvert_array_type arr :> Descriptor.for_field)
      | `Long | `Float | `Byte | `Char | `Boolean | `Int | `Short | `Double ->
          assert false
      end
  | Jlambda.Value_java_string _       -> `Class class_String

let rec compile_primitive ofs prim prim_args compile_expression_list compile_expression =
  let if_size = 3 in
  let goto_size = 3 in
  let lconst_size = 1 in
  let meth_size = 3 in
  let after_args_instrs l =
    node
      [ compile_expression_list ofs prim_args ;
        leaf l ] in
  let after_args_tree t =
    node
      [ compile_expression_list ofs prim_args ;
        t ] in
  let string_meth load meth =
    let params =
      if load then
        [`Class class_Value; `Class class_Value]
      else
        [`Class class_Value; `Class class_Value; `Class class_Value] in
    after_args_instrs
      [ Instruction.INVOKESTATIC
          (make_class "org.ocamljava.runtime.primitives.stdlib.Str",
           make_method meth,
           (params, `Class class_Value)) ] in
  let bigstring_meth load meth =
    let params =
      if load then
        [`Class class_Value; `Class class_Value]
      else
        [`Class class_Value; `Class class_Value; `Class class_Value] in
    after_args_instrs
      [ Instruction.INVOKESTATIC
          (make_class "org.ocamljava.runtime.primitives.otherlibs.bigarray.BigArray",
           make_method meth,
           (params, `Class class_Value)) ] in
  let fatal_error s =
    Misc.fatal_error ("Bytecodegen_prim.compile_primitive: " ^ s) in
  match prim, prim_args with
  | Pidentity, _ ->
      fatal_error "Pidentity"
  | Pignore, [arg] ->
      compile_expression false ofs arg
  | Pignore, _ ->
      assert false
  | Prevapply _, _ ->
      fatal_error "Prevapply"
  | Pdirapply _, _ ->
      fatal_error "Pdirapply"
  | Pgetglobal id, _ ->
      let field_name =
        match Ident.name id with
        | "caml_exn_Out_of_memory"              -> Some "exnOutOfMemory"
        | "caml_exn_Sys_error"                  -> Some "exnSysError"
        | "caml_exn_Failure"                    -> Some "exnFailure"
        | "caml_exn_Invalid_argument"           -> Some "exnInvalidArgument"
        | "caml_exn_End_of_file"                -> Some "exnEndOfFile"
        | "caml_exn_Division_by_zero"           -> Some "exnDivisionByZero"
        | "caml_exn_Not_found"                  -> Some "exnNotFound"
        | "caml_exn_Match_failure"              -> Some "exnMatchFailure"
        | "caml_exn_Stack_overflow"             -> Some "exnStackOverflow"
        | "caml_exn_Sys_blocked_io"             -> Some "exnSysBlockedIO"
        | "caml_exn_Assert_failure"             -> Some "exnAssertFailure"
        | "caml_exn_Undefined_recursive_module" -> Some "exnUndefinedRecursiveModule"
        | "caml_exn_Java_exception"             -> Some "exnJavaException"
        | "caml_exn_Java_error"                 -> Some "exnJavaError"
        | _ -> None in
      begin match field_name with
      | None ->
          snd (get_global id)
      | Some fn ->
          node
            [ meth_getPredefinedExceptions ;
              leaf [ Instruction.GETFIELD (class_PredefinedExceptions,
                                           make_field fn,
                                           `Class class_Value) ] ]
      end
  | Psetglobal id, _ ->
      begin try
        let cls = Jcompilenv.class_for_global id in
        after_args_tree
          (node
             [ leaf [ Instruction.GETSTATIC (make_class cls,
                                             make_field "GLOBALS",
                                             `Class class_ThreadLocal) ;
                      Instruction.SWAP ] ;
               meth_set_threadlocal ])
      with _ ->
        after_args_tree
          (node
             [ leaf [ Instruction.LDC_W (`String (UTF8.of_string (Ident.name id))); ];
               meth_setGlobal ])
      end
  | Pmakeblock (tag, mut), args ->
      compile_block (Int32.of_int tag) mut ofs args
        compile_expression_list compile_expression
  | Pfield idx, [Mprim (Pgetglobal id, _, _)] when (optimized_get_global id idx) ->
      begin match Jcompilenv.global_approx_no_dep id with
      | Jlambda.Value_tuple approx ->
          let glob_classname, get_global_instrs = get_global id in
          let fieldname = make_field (Printf.sprintf "field_%d" idx) in
          let desc = descriptor_of_approx approx.(idx) in
          node
            [ get_global_instrs ;
              leaf [ Instruction.GETFIELD (glob_classname, fieldname, desc) ] ]
      | _ -> assert false
      end
  | Pfield n, [arg] ->
      node
        [ compile_expression false ofs arg ;
          (match n with
          | 0 -> meth_get0
          | 1 -> meth_get1
          | 2 -> meth_get2
          | 3 -> meth_get3
          | 4 -> meth_get4
          | 5 -> meth_get5
          | 6 -> meth_get6
          | 7 -> meth_get7
          | _ ->
              if is_int32 n then
                node [ push_int32 (Int32.of_int n); meth_get'int ]
              else
                node [ push_int n; meth_get ]) ]
  | Pfield _, _ ->
      assert false
  | Psetfield (idx, _), [Mprim (Pgetglobal id, _, _); arg] when (optimized_get_global id idx) ->
      begin match Jcompilenv.global_approx_no_dep id with
      | Jlambda.Value_tuple approx ->
          let glob_classname, get_global_instrs = get_global id in
          let fieldname = make_field (Printf.sprintf "field_%d" idx) in
          let desc = descriptor_of_approx approx.(idx) in
          let arg_instrs = compile_expression false (ofs + 3 + meth_size) arg in
          node
            [ get_global_instrs ;
              arg_instrs ;
              leaf [ Instruction.PUTFIELD (glob_classname, fieldname, desc) ] ;
              field_Value_UNIT ]
      | _ -> assert false
      end
  | Psetfield (n, _), [arg1; arg2] ->
      let arg1_instrs, arg1_sz = with_size (compile_expression false ofs arg1) in
      let idx_instrs, idx_sz =
        with_size (if n < 8 then leaf []
                   else if is_int32 n then push_int32 (Int32.of_int n)
                   else push_int n) in
      let arg2_instrs = compile_expression false (ofs + arg1_sz + idx_sz) arg2 in
      node
        [ arg1_instrs ;
          idx_instrs ;
          arg2_instrs ;
          (match n with
          | 0 -> meth_set0
          | 1 -> meth_set1
          | 2 -> meth_set2
          | 3 -> meth_set3
          | 4 -> meth_set4
          | 5 -> meth_set5
          | 6 -> meth_set6
          | 7 -> meth_set7
          | _ -> if is_int32 n then meth_set'int else meth_set) ;
          field_Value_UNIT ]
  | Psetfield _, _ ->
      assert false
  | Pfloatfield n, [arg] ->
      node
        [ compile_expression false ofs arg ;
          (match n with
          | 0 -> meth_getGenericDouble0
          | 1 -> meth_getGenericDouble1
          | 2 -> meth_getGenericDouble2
          | 3 -> meth_getGenericDouble3
          | 4 -> meth_getGenericDouble4
          | 5 -> meth_getGenericDouble5
          | 6 -> meth_getGenericDouble6
          | 7 -> meth_getGenericDouble7
          | _ ->
              if is_int32 n then
                node [ push_int32 (Int32.of_int n); meth_getGenericDouble'int ]
              else
                node [ push_int n; meth_getGenericDouble ]) ]
  | Pfloatfield _, _ ->
      assert false
  | Psetfloatfield n, [arg1; arg2] ->
      let arg1_instrs, arg1_sz = with_size (compile_expression false ofs arg1) in
      let idx_instrs, idx_sz =
        with_size (if n < 8 then leaf []
                   else if is_int32 n then push_int32 (Int32.of_int n)
                   else push_int n) in
      let arg2_instrs = compile_expression false (ofs + arg1_sz + idx_sz) arg2 in
      node
        [ arg1_instrs ;
          idx_instrs ;
          arg2_instrs ;
          (match n with
          | 0 -> meth_setGenericDouble0
          | 1 -> meth_setGenericDouble1
          | 2 -> meth_setGenericDouble2
          | 3 -> meth_setGenericDouble3
          | 4 -> meth_setGenericDouble4
          | 5 -> meth_setGenericDouble5
          | 6 -> meth_setGenericDouble6
          | 7 -> meth_setGenericDouble7
          | _ -> if is_int32 n then meth_setGenericDouble'int else meth_setGenericDouble) ;
          field_Value_UNIT ]
  | Psetfloatfield _, _ ->
      assert false
  | Pduprecord _, _ ->
      fatal_error "Pduprecord"
  | Plazyforce, _ ->
      fatal_error "Plazyforce"
  | Pccall prim, _ ->
      let name = prim.Primitive.prim_name in
      if Macrogen_primitives.is_unary_math_primitive name then
        after_args_instrs
          [ Instruction.INVOKESTATIC (make_class "java.lang.Math",
                                      make_method (Macrogen_primitives.unary_method name),
                                      ([`Double], `Double)) ]
      else if Macrogen_primitives.is_binary_math_primitive name then
        after_args_instrs
          [ Instruction.INVOKESTATIC
              (make_class "java.lang.Math",
               make_method (Macrogen_primitives.binary_method name),
               ([`Double; `Double], `Double)) ]
      else begin
        let desc = Runtimeprimitives.get_description name in
        after_args_tree
          (node
             [ (if desc.Runtimeprimitives.primdesc_parameters = [] then
                 leaf [ Instruction.POP ]
               else
                 leaf []) ;
               leaf [ Instruction.INVOKESTATIC
                        (make_class desc.Runtimeprimitives.primdesc_class,
                         make_method desc.Runtimeprimitives.primdesc_method,
                         desc.Runtimeprimitives.primdesc_javadesc) ] ;
               (if desc.Runtimeprimitives.primdesc_return = LR_none then
                 field_Value_UNIT
               else
                 leaf []) ])
      end
  | Praise, [arg] ->
      node
        [ compile_expression false ofs arg ;
          meth_raise;
          field_Value_UNIT ]
  | Praise, _ ->
      assert false
  | Psequand, _ ->
      assert false
  | Psequor, _ ->
      assert false
  | Pnot, [arg] ->
      node
        [ compile_expression false ofs arg ;
          leaf [ Instruction.L2I ;
                 Instruction.IFEQ (Utils.s2 (if_size + lconst_size + goto_size)) ;
                 Instruction.LCONST_0 ;
                 Instruction.GOTO (Utils.s2 (goto_size + lconst_size)) ;
                 Instruction.LCONST_1 ] ]
  | Pnot, _ ->
      assert false
  | Pnegint, _ -> after_args_tree meth_negint
  | Paddint, [arg; Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int 1))))]
  | Paddint, [Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int 1)))); arg]
  | Paddint, [arg; Mconst (Jlambda.Lambda_const (Const_base (Const_int 1)))]
  | Paddint, [Mconst (Jlambda.Lambda_const (Const_base (Const_int 1))); arg] ->
      if inline_simple_arithmetic () then
        if Jconfig.ints_are_63_bit_long then
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LDC2_W (`Long 2L);
                     Instruction.LADD ] ]
        else
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LCONST_1 ;
                     Instruction.LADD ] ]
      else
        node
          [ compile_expression_list ofs [arg] ;
            meth_incrint ]
  | Paddint, [arg; Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1)))))]
  | Paddint, [Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1))))); arg]
  | Paddint, [arg; Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1))))]
  | Paddint, [Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1)))); arg] ->
      if inline_simple_arithmetic () then
        if Jconfig.ints_are_63_bit_long then
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LDC2_W (`Long 2L);
                     Instruction.LSUB ] ]
        else
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LCONST_1 ;
                     Instruction.LSUB ] ]
      else
        node
          [ compile_expression_list ofs [arg] ;
            meth_decrint ]
  | Paddint, _ ->
      if inline_simple_arithmetic () then
        if Jconfig.ints_are_63_bit_long then
          after_args_tree
            (leaf [ Instruction.LADD;
                    Instruction.LCONST_1;
                    Instruction.LSUB ])
        else
          after_args_tree
            (leaf [ Instruction.LADD ])
      else
        after_args_tree meth_addint
  | Psubint, [arg; Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int 1))))]
  | Psubint, [arg; Mconst (Jlambda.Lambda_const (Const_base (Const_int 1)))] ->
      if inline_simple_arithmetic () then
        if Jconfig.ints_are_63_bit_long then
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LDC2_W (`Long 2L);
                     Instruction.LSUB ] ]
        else
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LCONST_1 ;
                     Instruction.LSUB ] ]
      else
        node
          [ compile_expression_list ofs [arg] ;
            meth_decrint ]
  | Psubint, [arg; Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1)))))]
  | Psubint, [arg; Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1))))] ->
      if inline_simple_arithmetic () then
        if Jconfig.ints_are_63_bit_long then
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LDC2_W (`Long 2L);
                     Instruction.LADD ] ]
        else
          node
            [ compile_expression_list ofs [arg] ;
              leaf [ Instruction.LCONST_1 ;
                     Instruction.LADD ] ]
      else
        node
          [ compile_expression_list ofs [arg] ;
            meth_incrint ]
  | Psubint, _ ->
      if inline_simple_arithmetic () then
        if Jconfig.ints_are_63_bit_long then
          after_args_tree
            (leaf [ Instruction.LSUB;
                    Instruction.LCONST_1;
                    Instruction.LADD ])
        else
          after_args_tree
            (leaf [ Instruction.LSUB ])
      else
        after_args_tree meth_subint
  | Pmulint, _ -> after_args_tree meth_mulint
  | Pdivint, _ -> after_args_tree meth_divint
  | Pmodint, _ -> after_args_tree meth_modint
  | Pandint, _ -> after_args_instrs [ Instruction.LAND ]
  | Porint, _ -> after_args_instrs [ Instruction.LOR ]
  | Pxorint, _ ->
      if Jconfig.ints_are_63_bit_long then
        after_args_instrs [ Instruction.LXOR; Instruction.LCONST_1; Instruction.LOR ]
      else
        after_args_instrs [ Instruction.LXOR ]
  | Plslint, _ -> after_args_tree meth_lslint
  | Plsrint, _ -> after_args_tree meth_lsrint
  | Pasrint, _ -> after_args_tree meth_asrint
  | Pintcomp cmp, [arg0; arg1; arg2] ->
      begin match arg0 with
      | Mconst (Jlambda.Lambda_const (Const_base (Const_int 1))) (* tagged values *)
      | Mconst (Jlambda.Lambda_const (Const_base (Const_int 2))) (* normalized values *)
      | Mconst (Jlambda.Lambda_const (Const_base (Const_int 3))) -> (* unboxed values *)
          let jump_ofs = if_size + lconst_size + goto_size in
          node
            [ compile_expression_list ofs [arg1; arg2] ;
              leaf [ Instruction.LCMP ;
                     (match cmp with
                     | Ceq -> Instruction.IFEQ (Utils.s2 jump_ofs)
                     | Cneq -> Instruction.IFNE (Utils.s2 jump_ofs)
                     | Clt -> Instruction.IFLT (Utils.s2 jump_ofs)
                     | Cgt -> Instruction.IFGT (Utils.s2 jump_ofs)
                     | Cle -> Instruction.IFLE (Utils.s2 jump_ofs)
                     | Cge -> Instruction.IFGE (Utils.s2 jump_ofs)) ;
                     Instruction.LCONST_0 ;
                     Instruction.GOTO (Utils.s2 (goto_size + lconst_size)) ;
                     Instruction.LCONST_1 ] ]
      | _ -> (* boxed values *)
          node
            [ compile_expression_list ofs [arg1; arg2] ;
              (match cmp with
              | Ceq -> meth_equalValues
              | Cneq -> meth_notEqualValues
              | Clt -> meth_lowerThanValue
              | Cgt -> meth_greaterThanValue
              | Cle -> meth_lowerEqualValue
              | Cge -> meth_greaterEqualValue) ;
              leaf [ Instruction.I2L ] ]
      end
  | Pintcomp _, _ ->
      assert false
  | Poffsetint 1, _ ->
      after_args_tree (node [ meth_incrint ])
  | Poffsetint (-1), _ ->
      after_args_tree (node [ meth_decrint ])
  | Poffsetint n, _ ->
      if Jconfig.ints_are_63_bit_long then
        after_args_tree (node [ push_int ((n lsl 1) lor 1); meth_offsetint ])
      else
        after_args_tree (node [ push_int n; meth_offsetint ])
  | Poffsetref n, _ ->
      after_args_tree (node [ push_int (n * 2); meth_offsetref; field_Value_UNIT ])
  | Pintoffloat, _ -> after_args_instrs [ Instruction.D2L ]
  | Pfloatofint, _ -> after_args_instrs [ Instruction.L2D ]
  | Pnegfloat, _ -> after_args_instrs [ Instruction.DNEG ]
  | Pabsfloat, _ -> after_args_tree meth_abs
  | Paddfloat, _ -> after_args_instrs [ Instruction.DADD ]
  | Psubfloat, _ -> after_args_instrs [ Instruction.DSUB ]
  | Pmulfloat, _ -> after_args_instrs [ Instruction.DMUL ]
  | Pdivfloat, _ -> after_args_instrs [ Instruction.DDIV ]
  | Pfloatcomp cmp, _ ->
      let jump_ofs = if_size + lconst_size + goto_size in
      after_args_instrs
        [ Instruction.DCMPG ;
          (match cmp with
          | Ceq -> Instruction.IFEQ (Utils.s2 jump_ofs)
          | Cneq -> Instruction.IFNE (Utils.s2 jump_ofs)
          | Clt -> Instruction.IFLT (Utils.s2 jump_ofs)
          | Cgt -> Instruction.IFGT (Utils.s2 jump_ofs)
          | Cle -> Instruction.IFLE (Utils.s2 jump_ofs)
          | Cge -> Instruction.IFGE (Utils.s2 jump_ofs)) ;
          Instruction.LCONST_0 ;
          Instruction.GOTO (Utils.s2 (goto_size + lconst_size)) ;
          Instruction.LCONST_1 ]
  | Pstringlength, _ ->
      after_args_tree meth_sizeBytes;
  | Pstringrefu, _ ->
      after_args_tree
        (node [ meth_getUnsignedByte;
                leaf [ Instruction.I2L ] ]);
  | Pstringsetu, _ ->
      after_args_tree
        (node [ leaf [ Instruction.L2I ] ;
                meth_setUnsignedByte ;
                field_Value_UNIT ])
  | Pstringrefs, _ ->
      after_args_tree meth_caml_string_get
  | Pstringsets, _ ->
      after_args_tree meth_caml_string_set
  | Pmakearray _, [] ->
      Bytecodegen_constants.push_structured_constant (Const_block (0, []))
  | Pmakearray kind, args ->
      (match kind with
      | Paddrarray ->
          compile_block 0l Mutable ofs args compile_expression_list compile_expression
      | Pintarray ->
          let longs n =
            let rec lng acc = function
              | 0 -> (`Int :: acc, (`Class class_Value))
              | n -> lng (`Long :: acc) (pred n) in
            lng [] n in
          let args_size = List.length args in
          if args_size <= 8 then
            node
              [ leaf [ Instruction.ICONST_0 ] ;
                compile_long_expression_list (succ ofs) args compile_expression ;
                leaf [ Instruction.INVOKESTATIC
                         (class_Value,
                          make_method "createLongBlock",
                          longs args_size) ] ]
          else
            let dup_size = 1 in
            let alloc_instr, alloc_sz =
              with_size
                (node [ leaf [ Instruction.ICONST_0 ] ;
                        push_int args_size ;
                        meth_createLongBlockFromSize ]) in
            let _, _, set_instrs =
              List.fold_left
                (fun (idx, ofs, acc) elem ->
                  let idx_instr, idx_sz = with_size (push_int idx) in
                  let instrs =
                    node
                      [ leaf [ Instruction.DUP ] ;
                        idx_instr ;
                        compile_expression false (ofs + dup_size + idx_sz) elem ;
                        meth_setRawLong ] in
                  (succ idx, ofs + (size instrs), instrs :: acc))
                (0, ofs + alloc_sz, [])
                args in
            node
              [ alloc_instr ;
                node (List.rev set_instrs) ]
      | Pfloatarray ->
          let doubles n =
            let rec dbl acc = function
              | 0 -> (acc, (`Class class_Value))
              | n -> dbl (`Double :: acc) (pred n) in
            dbl [] n in
          let args_size = List.length args in
          if args_size <= 8 then
            node
              [ compile_double_expression_list ofs args compile_expression ;
                leaf [ Instruction.INVOKESTATIC
                         (class_Value,
                          make_method "createDoubleArray",
                          doubles args_size) ] ]
          else
            let dup_size = 1 in
            let alloc_instr, alloc_sz =
              with_size
                (node [ push_int args_size ;
                        meth_createDoubleArray ]) in
            let _, _, set_instrs =
              List.fold_left
                (fun (idx, ofs, acc) elem ->
                  let idx_instr, idx_sz = with_size (push_int idx) in
                  let instrs =
                    node
                      [ leaf [ Instruction.DUP ] ;
                        idx_instr ;
                        compile_expression false (ofs + dup_size + idx_sz) elem ;
                        meth_setDouble ] in
                  (succ idx, ofs + (size instrs), instrs :: acc))
                (0, ofs + alloc_sz, [])
                args in
            node
              [ alloc_instr ;
                node (List.rev set_instrs) ]
      | Pgenarray ->
          node
            [ compile_block 0l Mutable ofs args compile_expression_list compile_expression ;
              meth_caml_make_array ])
  | Parraylength _, _ ->
      after_args_tree meth_arrayLength
  | Parrayrefu Paddrarray, [Mconst (Jlambda.Lambda_const (Const_base (Const_int idx)))] when (is_int32 idx) ->
      node
        [ push_int32 (Int32.of_int idx) ;
          meth_get'int ]
  | Parrayrefu kind, _ ->
      after_args_tree
        (match kind with
        | Pgenarray -> meth_caml_array_unsafe_get
        | Pfloatarray -> meth_getDouble
        | Pintarray -> meth_getRawLong
        | Paddrarray -> meth_get)
  | Parraysetu kind, _ ->
      after_args_tree
        (match kind with
        | Pgenarray -> meth_caml_array_unsafe_set
        | Pfloatarray -> node [ meth_setDouble ; field_Value_UNIT ]
        | Pintarray -> node [ meth_setRawLong ; field_Value_UNIT ]
        | Paddrarray -> node [ meth_set ; field_Value_UNIT ])
  | Parrayrefs kind, _ ->
      after_args_tree
        (match kind with
        | Pgenarray -> meth_caml_array_get
        | Pfloatarray -> meth_caml_array_get_float
        | Pintarray -> meth_caml_array_get_int
        | Paddrarray -> meth_caml_array_get_addr)
  | Parraysets kind, _ ->
      after_args_tree
        (match kind with
        | Pgenarray -> meth_caml_array_set
        | Pfloatarray -> meth_caml_array_set_float
        | Pintarray -> meth_caml_array_set_int
        | Paddrarray -> meth_caml_array_set_addr)
  | Pisint, _ ->
      after_args_tree
        (node
           [ meth_isLong ;
             leaf [ Instruction.I2L ] ])
  | Pisout, [arg0; arg1] ->
      let can_be_represented_int x =
        (x lsr 62) = 0 in
      let can_be_represented_target_int x =
        (Targetint.compare
           (Targetint.shift_right_logical x (Targetint.of_int 62))
           Targetint.zero) = 0 in
      let meth =
        match arg0, arg1 with
        | Mconst (Jlambda.Lambda_const (Const_base (Const_int x))), _
        | Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int x)))), _
          when can_be_represented_int x ->
            meth_isOutFirstPositive
        | Mconst (Jlambda.Const_targetint x), _
        | Mconvert (_, _, Mconst (Jlambda.Const_targetint x)), _
          when can_be_represented_target_int x ->
            meth_isOutFirstPositive
        | _, Mconst (Jlambda.Lambda_const (Const_base (Const_int x)))
        | _, Mconvert (_, _, Mconst (Jlambda.Lambda_const (Const_base (Const_int x))))
          when can_be_represented_int x ->
            meth_isOutSecondPositive
        | _, Mconst (Jlambda.Const_targetint x)
        | _, Mconvert (_, _, Mconst (Jlambda.Const_targetint x))
          when can_be_represented_target_int x ->
            meth_isOutSecondPositive
        | _ ->
            meth_isOut in
      after_args_tree
        (node
           [ meth ;
             leaf [ Instruction.I2L ] ])
  | Pisout, _ ->
      assert false
  | Pbittest, _ ->
      after_args_tree
        (node
           [ meth_bitvect_test;
             leaf [ Instruction.I2L ] ])
  | Pbintofint bi, _ ->
      after_args_tree (compile_conversion int_kind (kind_of_boxed_integer bi))
  | Pintofbint bi, _ ->
      after_args_tree (compile_conversion (kind_of_boxed_integer bi) int_kind)
  | Pcvtbint (src, dst), _ ->
      let src_kind = kind_of_boxed_integer src in
      let dst_kind = kind_of_boxed_integer dst in
      after_args_tree (compile_conversion src_kind dst_kind)
  | Pnegbint Pnativeint, _ -> after_args_instrs [ Instruction.LNEG ]
  | Pnegbint Pint32, _ -> after_args_instrs [ Instruction.INEG ]
  | Pnegbint Pint64, _ -> after_args_instrs [ Instruction.LNEG ]
  | Paddbint Pnativeint, _ -> after_args_instrs [ Instruction.LADD ]
  | Paddbint Pint32, _ -> after_args_instrs [ Instruction.IADD ]
  | Paddbint Pint64, _ -> after_args_instrs [ Instruction.LADD ]
  | Psubbint Pnativeint, _ -> after_args_instrs [ Instruction.LSUB ]
  | Psubbint Pint32, _ -> after_args_instrs [ Instruction.ISUB ]
  | Psubbint Pint64, _ -> after_args_instrs [ Instruction.LSUB ]
  | Pmulbint Pnativeint, _ -> after_args_instrs [ Instruction.LMUL ]
  | Pmulbint Pint32, _ -> after_args_instrs [ Instruction.IMUL ]
  | Pmulbint Pint64, _ -> after_args_instrs [ Instruction.LMUL ]
  | Pdivbint Pnativeint, _ -> after_args_tree meth_divint64
  | Pdivbint Pint32, _ -> after_args_tree meth_divint32
  | Pdivbint Pint64, _ -> after_args_tree meth_divint64
  | Pmodbint Pnativeint, _ -> after_args_tree meth_modint64
  | Pmodbint Pint32, _ -> after_args_tree meth_modint32
  | Pmodbint Pint64, _ -> after_args_tree meth_modint64
  | Pandbint Pnativeint, _ -> after_args_instrs [ Instruction.LAND ]
  | Pandbint Pint32, _ -> after_args_instrs [ Instruction.IAND ]
  | Pandbint Pint64, _ -> after_args_instrs [ Instruction.LAND ]
  | Porbint Pnativeint, _ -> after_args_instrs [ Instruction.LOR ]
  | Porbint Pint32, _ -> after_args_instrs [ Instruction.IOR ]
  | Porbint Pint64, _ -> after_args_instrs [ Instruction.LOR ]
  | Pxorbint Pnativeint, _ -> after_args_instrs [ Instruction.LXOR ]
  | Pxorbint Pint32, _ -> after_args_instrs [ Instruction.IXOR ]
  | Pxorbint Pint64, _ -> after_args_instrs [ Instruction.LXOR ]
  | Plslbint Pnativeint, _ -> after_args_instrs [ Instruction.L2I ; Instruction.LSHL ]
  | Plslbint Pint32, _ -> after_args_instrs [ Instruction.L2I ; Instruction.ISHL ]
  | Plslbint Pint64, _ -> after_args_instrs [ Instruction.L2I ; Instruction.LSHL ]
  | Plsrbint Pnativeint, _ -> after_args_instrs [ Instruction.L2I ; Instruction.LUSHR ]
  | Plsrbint Pint32, _ -> after_args_instrs [ Instruction.L2I ; Instruction.IUSHR ]
  | Plsrbint Pint64, _ -> after_args_instrs [ Instruction.L2I ; Instruction.LUSHR ]
  | Pasrbint Pnativeint, _ -> after_args_instrs [ Instruction.L2I ; Instruction.LSHR ]
  | Pasrbint Pint32, _ -> after_args_instrs [ Instruction.L2I ; Instruction.ISHR ]
  | Pasrbint Pint64, _ -> after_args_instrs [ Instruction.L2I ; Instruction.LSHR ]
  | Pbintcomp (Pint32, cmp), _ ->
      let jump_ofs = if_size + lconst_size + goto_size in
      after_args_instrs
        [ (match cmp with
          | Ceq -> Instruction.IF_ICMPEQ (Utils.s2 jump_ofs)
          | Cneq -> Instruction.IF_ICMPNE (Utils.s2 jump_ofs)
          | Clt -> Instruction.IF_ICMPLT (Utils.s2 jump_ofs)
          | Cgt -> Instruction.IF_ICMPGT (Utils.s2 jump_ofs)
          | Cle -> Instruction.IF_ICMPLE (Utils.s2 jump_ofs)
          | Cge -> Instruction.IF_ICMPGE (Utils.s2 jump_ofs)) ;
          Instruction.LCONST_0 ;
          Instruction.GOTO (Utils.s2 (goto_size + lconst_size)) ;
          Instruction.LCONST_1 ]
  | Pbintcomp (Pnativeint, cmp), _
  | Pbintcomp (Pint64, cmp), _ ->
      let jump_ofs = if_size + lconst_size + goto_size in
      after_args_instrs
        [ Instruction.LCMP ;
          (match cmp with
          | Ceq -> Instruction.IFEQ (Utils.s2 jump_ofs)
          | Cneq -> Instruction.IFNE (Utils.s2 jump_ofs)
          | Clt -> Instruction.IFLT (Utils.s2 jump_ofs)
          | Cgt -> Instruction.IFGT (Utils.s2 jump_ofs)
          | Cle -> Instruction.IFLE (Utils.s2 jump_ofs)
          | Cge -> Instruction.IFGE (Utils.s2 jump_ofs)) ;
          Instruction.LCONST_0 ;
          Instruction.GOTO (Utils.s2 (goto_size + lconst_size)) ;
          Instruction.LCONST_1 ]
  | Pbigarrayref _, _ ->
      fatal_error "Pbigarrayref"
  | Pbigarrayset _, _ ->
      fatal_error "Pbigarrayset"
  | Pbigarraydim ((1 | 2 | 3) as dim), _ ->
      after_args_instrs
        [ Instruction.INVOKESTATIC (make_class "org.ocamljava.runtime.primitives.otherlibs.bigarray.BigArray",
                                    make_method ("caml_ba_dim_" ^ (string_of_int dim)),
                                    ([`Class class_Value], `Class class_Value)) ]
  | Pbigarraydim _, _ ->
      assert false
  | Pstring_load_16 _, _ ->
      string_meth true "caml_string_get16"
  | Pstring_load_32 _, _ ->
      string_meth true "caml_string_get32"
  | Pstring_load_64 _, _ ->
      string_meth true "caml_string_get64"
  | Pstring_set_16 _, _ ->
      string_meth false "caml_string_set16"
  | Pstring_set_32 _, _ ->
      string_meth false "caml_string_set32"
  | Pstring_set_64 _, _ ->
      string_meth false "caml_string_set64"
  | Pbigstring_load_16 _, _ ->
      bigstring_meth true "caml_ba_uint8_get16"
  | Pbigstring_load_32 _, _ ->
      bigstring_meth true "caml_ba_uint8_get32"
  | Pbigstring_load_64 _, _ ->
      bigstring_meth true "caml_ba_uint8_get64"
  | Pbigstring_set_16 _, _ ->
      bigstring_meth false "caml_ba_uint8_set16"
  | Pbigstring_set_32 _, _ ->
      bigstring_meth false "caml_ba_uint8_set32"
  | Pbigstring_set_64 _, _ ->
      bigstring_meth false "caml_ba_uint8_set64"
  | Pctconst Big_endian, _ ->
      fatal_error "Pctconst Big_endian"
  | Pctconst Word_size, _ ->
      fatal_error "Pctconst Word_size"
  | Pctconst Ostype_unix, _ ->
      fatal_error "Pctconst Ostype_unix"
  | Pctconst Ostype_win32, _ ->
      fatal_error "Pctconst Ostype_win32"
  | Pctconst Ostype_cygwin, _ ->
      fatal_error "Pctconst Ostype_cygwin"
  | Pbswap16, _ ->
      after_args_instrs
        [ Instruction.I2S ;
          Instruction.INVOKESTATIC (make_class "java.lang.Short",
                                    make_method "reverseBytes",
                                    ([`Short], `Short)) ]
  | Pbbswap Pint32, _ ->
      after_args_instrs
        [ Instruction.INVOKESTATIC (make_class "java.lang.Integer",
                                    make_method "reverseBytes",
                                    ([`Int], `Int)) ]
  | Pbbswap Pint64, _
  | Pbbswap Pnativeint, _ ->
      after_args_instrs
        [ Instruction.INVOKESTATIC (make_class "java.lang.Long",
                                    make_method "reverseBytes",
                                    ([`Long], `Long)) ]

and compile_block tag mut ofs args compile_expression_list compile_expression =
  let args_size = List.length args in
  if args_size = 0 && mut = Asttypes.Immutable then
    node
      [ push_int32 tag ;
        meth_getAtom ]
  else
    if args_size <= 8 then
      let int_instr, int_sz = with_size (push_int32 tag) in
      node
        [ int_instr ;
          compile_expression_list (ofs + int_sz) args ;
          leaf
            [ Instruction.INVOKESTATIC
                (class_Value,
                 make_method "createBlock",
                 (`Int :: (repeat_parameters args_size), `Class class_Value)) ] ]
    else
      let dup_size = 1 in
      let alloc_instr, alloc_sz =
        with_size
          (node
             [ push_int32 tag ;
               push_int args_size ;
               meth_createBlock ]) in
      let _, _, set_instrs =
        List.fold_left
          (fun (idx, ofs, acc) elem ->
            let idx_instr, idx_sz = with_size (push_int idx) in
            let instrs =
              node
                [ leaf [ Instruction.DUP ] ;
                  idx_instr ;
                  compile_expression false (ofs + dup_size + idx_sz) elem ;
                  meth_set ] in
            (succ idx, ofs + (size instrs), instrs :: acc))
          (0, ofs + alloc_sz, [])
          args in
      node
        [ alloc_instr ;
          node (List.rev set_instrs) ]

and compile_long_expression_list ofs l compile_expression =
  List.fold_left
    (fun (ofs, acc) elem ->
      let res, sz = with_size (compile_expression false ofs elem) in
      ofs + sz, res :: acc)
    (ofs, [])
    l
  |> snd
  |> List.rev
  |> node

and compile_double_expression_list ofs l compile_expression =
  List.fold_left
    (fun (ofs, acc) elem ->
      let res, sz = with_size (compile_expression false ofs elem) in
      ofs + sz, res :: acc)
    (ofs, [])
    l
  |> snd
  |> List.rev
  |> node
