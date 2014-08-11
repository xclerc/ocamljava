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

open Macroinstr
open Bytecodeutils
open Lambda
open Asttypes
open BaristaLibrary
open Bytecodegen_misc
open Bytecodegen_javaprim
open Bytecodegen_prim


(* expression compilation *)

let node x = Instrtree.node x

let leaf x = Instrtree.leaf x

let size x = Instrtree.size x

let with_size x = x, size x

let compile_signals () =
  if !Jclflags.signals then
    meth_checkSignals
  else
    leaf [ ]

let push_method_handle class_name func_name arity =
  let class_name = make_class class_name in
  let meth_name = Name.make_for_method func_name in
  let meth_desc = repeat_parameters arity, `Class class_Value in
  let meth_ref = class_name, meth_name, meth_desc in
  leaf [ Instruction.LDC_W (`Method_handle (`invokeStatic meth_ref)) ]

let constant_closures : (Name.for_field * Instrtree.t) list ref = ref []

let add_constant_closure instrs =
  let name =
    !constant_closures
    |> List.length
    |> Printf.sprintf "CLOSURE_%d"
    |> make_field in
  constant_closures := (name, instrs) :: !constant_closures;
  leaf [ Instruction.GETSTATIC (make_class (State.current_class ()),
                                name,
                                `Class class_Value) ]

let add_simple_constant_closure cls mth jarity oarity =
  let instrs =
    match oarity with
    | None ->
        node
        [ push_method_handle cls (normalize_function_name mth) jarity ;
          meth_createClosure1 ]
    | Some arity ->
        node
        [ push_method_handle cls (normalize_function_name mth) jarity ;
          push_int32 (Int32.of_int arity) ;
          meth_createClosureN ] in
  add_constant_closure instrs

let rec compile_expression is_tail ofs curr_expr =
  let meth_size = 3 in
  let if_size = 3 in
  let goto_size = 3 in
  match curr_expr with
  | Mconst (Jlambda.Lambda_const lc) ->
      Bytecodegen_constants.push_structured_constant lc
  | Mconst (Jlambda.Const_targetint x) ->
      Bytecodeutils.push_int64 (Targetint.to_int64 x)
  | Mconst (Jlambda.Const_null _) ->
      leaf [ Instruction.ACONST_NULL ]
  | Mconst (Jlambda.Const_javastring s) ->
      leaf [ Instruction.LDC_W (`String (UTF8.of_string s)) ]
  | Moffset (expr, n) ->
      node
        [ compile_expression false ofs expr ;
          push_int n ;
          meth_offset ]
  | Mdynamicoffset (expr, n) ->
      let expr_instrs, expr_sz = with_size (compile_expression false ofs expr) in
      node
        [ expr_instrs ;
          meth_get0 ;
          (match n with
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 0))) -> meth_get0
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 1))) -> meth_get1
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 2))) -> meth_get2
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 3))) -> meth_get3
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 4))) -> meth_get4
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 5))) -> meth_get5
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 6))) -> meth_get6
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int 7))) -> meth_get7
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 0L -> meth_get0
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 1L -> meth_get1
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 2L -> meth_get2
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 3L -> meth_get3
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 4L -> meth_get4
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 5L -> meth_get5
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 6L -> meth_get6
          | Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 7L -> meth_get7
          | Mconst (Jlambda.Lambda_const (Const_base (Const_int idx))) when is_int32 idx ->
              node
                [ push_int32 (Int32.of_int idx) ;
                  meth_get'int ]
          | Mconst (Jlambda.Const_targetint idx) when is_int32 (Targetint.to_int idx) ->
              node
                [ push_int32 (Targetint.to_int32 idx) ;
                  meth_get'int ]
          | _ ->
              node
                [ compile_expression false (ofs + expr_sz + meth_size) n ;
                  meth_get ]) ]
  | Mcreateglobal (id, classname) ->
      node
        [ leaf [ Instruction.LDC_W (`String (UTF8.of_string id));
                 Instruction.NEW (make_class classname);
                 Instruction.DUP;
                 Instruction.INVOKESPECIAL (make_class classname,
                                            make_method "<init>",
                                            ([], `Void));
                 Instruction.DUP_X1 ];
          meth_setGlobal ]
  | Mclosure (2, [{ mfun_label = { Jlambda.fl_class; fl_method };
                    mfun_ocaml_arity = 1;
                    mfun_java_arity }], []) ->
      add_simple_constant_closure fl_class fl_method mfun_java_arity None
  | Mclosure (3, [{ mfun_label = { Jlambda.fl_class; fl_method };
                    mfun_ocaml_arity;
                    mfun_java_arity }], []) ->
      add_simple_constant_closure fl_class fl_method mfun_java_arity (Some mfun_ocaml_arity)
  | Mclosure (sz, { mfun_label = { Jlambda.fl_class; fl_method };
                    mfun_ocaml_arity;
                    mfun_java_arity } :: others, []) ->
      let size_instrs = push_int sz in
      let first_instrs, next =
        compile_first_closure fl_class fl_method mfun_ocaml_arity mfun_java_arity in
      let _, others_instrs =
        compile_other_closures next others in
      let instrs =
        node
          [ size_instrs ;
            meth_createClosure ;
            first_instrs ;
            others_instrs ] in
      add_constant_closure instrs
  | Mclosure (sz, { mfun_label = { Jlambda.fl_class; fl_method };
                    mfun_ocaml_arity;
                    mfun_java_arity } :: others, vars) ->
      let size_instrs = push_int sz in
      let first_instrs, next =
        compile_first_closure fl_class fl_method mfun_ocaml_arity mfun_java_arity in
      let index, others_instrs =
        compile_other_closures next others in
      let ofs =
        ofs + (size size_instrs) + meth_size + (size first_instrs) + (size others_instrs) in
      let vars_instrs =
        compile_expression_list_to_value ofs index vars in
      node
        [ size_instrs ;
          meth_createClosure ;
          first_instrs ;
          others_instrs ;
          vars_instrs ]
  | Mclosure _ ->
      Misc.fatal_error "Bytecodegen.compile_expression: invalid closure"
  | Mreadlocal (k, idx) ->
      kload k idx
  | Mwritelocal (k, idx, expr) ->
      node [ compile_expression false ofs expr ; kstore k idx ]
  | Mpoptolocal (k, idx) ->
      kstore k idx
  | Mcall ({ Jlambda.fl_class; fl_method }, args, arg_kinds, ret_kind, dbg) ->
      State.add_debug_info ofs dbg;
      if is_tail && (State.is_same_function fl_class fl_method) then
        let args_instrs, args_sz =
          with_size (compile_expression_list_to_locals ofs args arg_kinds) in
        let dest = ~-(ofs + args_sz) in
        node
          [ args_instrs ;
            leaf [ if dest >= -32768 then
              Instruction.GOTO (Utils.s2 dest)
            else
              Instruction.GOTO_W (Utils.s4 (Int32.of_int dest)) ] ]
      else begin
        let type_of_kind = function
          | Boxed_value            -> `Class class_Value
          | Tagged_int             -> `Long
          | Normalized_int         -> `Long
          | Unboxed_int            -> `Long
          | Unboxed_int32          -> `Int
          | Unboxed_int64          -> `Long
          | Unboxed_nativeint      -> `Long
          | Unboxed_float          -> `Double
          | Unboxed_instance cn    -> `Class (make_class cn)
          | Unboxed_java_array arr -> (Jlambda.unconvert_array_type arr :> Descriptor.for_parameter) in
        let ret_type =
          match ret_kind with
          | None   -> `Void
          | Some k -> (type_of_kind k :> Descriptor.java_type) in
        let params = List.map type_of_kind arg_kinds in
        node
          [ compile_expression_list ofs args ;
            leaf [ Instruction.INVOKESTATIC
                     (make_class fl_class,
                      make_method fl_method,
                      (params, ret_type)) ] ]
      end
  | Mprim (prim, args, dbg) ->
      State.add_debug_info ofs dbg;
      compile_primitive
        ofs prim args
        compile_expression_list compile_expression
  | Mjavaprim (jprim, args, dbg) ->
      State.add_debug_info ofs dbg;
      compile_java_primitive
        ofs jprim args
        compile_expression_list compile_expression
  | Mapply (clos, args, dbg) ->
      (* compilation scheme:
           aload <clos>
           <args>
           invokestatic NativeApply.send(Value,...)
           <test signals> *)
      State.add_debug_info ofs dbg;
      let load_instrs, load_sz = with_size (aload clos) in
      let len = List.length args in
      let args_instrs =
        if (len >= 1) && (len <= 8) then
          compile_expression_list (ofs + load_sz) args
        else
          compile_expression_list_to_array (ofs + load_sz) args in
      node
        [ load_instrs ;
          args_instrs ;
          (match len with
          | 1 -> meth_apply1
          | 2 -> meth_apply2
          | 3 -> meth_apply3
          | 4 -> meth_apply4
          | 5 -> meth_apply5
          | 6 -> meth_apply6
          | 7 -> meth_apply7
          | 8 -> meth_apply8
          | _ -> meth_apply) ;
          compile_signals () ]
  | Msend (tag, cache, pos, args, dbg) ->
      (* compilation scheme:
           <tag>
           <cache>
           <pos>
           <args>
           invokestatic NativeApply.send(Value,Value,Value,...)
           <test signals> *)
      State.add_debug_info ofs dbg;
      let tag_instrs, tag_sz = with_size (aload tag) in
      let cache_instrs, cache_sz = with_size (aload cache) in
      let pos_instrs, pos_sz = with_size (aload pos) in
      let len = List.length args in
      let args_instrs =
        if (len >= 0) && (len <= 8) then
          compile_expression_list (ofs + tag_sz + cache_sz + pos_sz) args
        else
          compile_expression_list_to_array (ofs + tag_sz + cache_sz + pos_sz) args in
      node
        [ tag_instrs ;
          cache_instrs ;
          pos_instrs ;
          args_instrs ;
          (match len with
          | 1 -> meth_send1
          | 2 -> meth_send2
          | 3 -> meth_send3
          | 4 -> meth_send4
          | 5 -> meth_send5
          | 6 -> meth_send6
          | 7 -> meth_send7
          | 8 -> meth_send8
          | _ -> meth_send) ;
          compile_signals () ]
  | Msequence (e1, e2) ->
      (* compilation scheme:
           <e1>
           <e2> *)
      let e1_instrs, e1_sz = with_size (compile_expression false ofs e1) in
      let e2_instrs = compile_expression is_tail (ofs + e1_sz) e2 in
      node
        [ e1_instrs ;
          e2_instrs ]
  | Mifthenelse (Mconvert (Boxed_value, (Normalized_int | Unboxed_int), cond),
                 ifso,
                 ifnot) ->
      (* compilation scheme:
           <cond> # supposed to push a long
           l2i
           ifeq else
           <ifso>
           goto end
         else:
           <ifnot>
         end: *)
      let cond_instrs, cond_sz =
        with_size (compile_expression false ofs cond) in
      let ifso_instrs, ifso_sz =
        with_size (compile_expression is_tail (ofs + cond_sz + 3 + if_size) ifso) in
      let ifnot_instrs, ifnot_sz =
        with_size (compile_expression is_tail (ofs + cond_sz + 3 + if_size + ifso_sz + goto_size) ifnot) in
      node
        [ cond_instrs ;
          field_Value_ZERO ;
          leaf [ Instruction.IF_ACMPEQ (check_jump (if_size + ifso_sz + goto_size)) ] ;
          ifso_instrs ;
          leaf [ Instruction.GOTO (check_jump (goto_size + ifnot_sz)) ] ;
          ifnot_instrs ]
  | Mifthenelse (cond, ifso, ifnot) ->
      (* compilation scheme:
           <cond> # supposed to push a long
           l2i
           ifeq else
           <ifso>
           goto end
         else:
           <ifnot>
         end: *)
      let cond_instrs, cond_sz =
        with_size (compile_expression false ofs cond) in
      let ifso_instrs, ifso_sz =
        with_size (compile_expression is_tail (ofs + cond_sz + 1 + if_size) ifso) in
      let ifnot_instrs, ifnot_sz =
        with_size (compile_expression is_tail (ofs + cond_sz + 1 + if_size + ifso_sz + goto_size) ifnot) in
      node
        [ cond_instrs ;
          leaf [ Instruction.L2I ;
                 Instruction.IFEQ (check_jump (if_size + ifso_sz + goto_size)) ] ;
          ifso_instrs ;
          leaf [ Instruction.GOTO (check_jump (goto_size + ifnot_sz)) ] ;
          ifnot_instrs ]
  | Mwhile (cond, body, Some times) ->
      (* compilation scheme:
           <cond> # supposed to push a long
           l2i
           ifeq end
         start:
           <sign_1>
           <body_1>
           <cond_1> # supposed to push a long
           l2i
           ifeq end
           ...
           <sign_n>
           <body_n>
           <cond_n> # supposed to push a long
           l2i
           ifne start
         end: *)
      let fisrtcond_instrs, firstcond_sz =
        with_size (compile_expression false ofs cond) in
      let start_offset = ofs + firstcond_sz + 1 + if_size in
      let curr_ofs = ref start_offset in
      let curr_body = ref [] in
      let ifeq_offsets = ref [] in
      for i = 1 to times do
        let sign_instrs, sign_sz = with_size (compile_signals ()) in
        let body_instrs, body_sz =
          with_size (compile_expression false (!curr_ofs + sign_sz) body) in
        let cond_instrs, cond_sz =
          with_size (compile_expression false (!curr_ofs + sign_sz + body_sz) cond) in
        let if_offset = !curr_ofs + sign_sz + body_sz + cond_sz + 1 in
        let jump_instrs, jump_sz =
          if i < times then begin
            (* store the current offset, to be patched later *)
            ifeq_offsets := if_offset :: !ifeq_offsets;
            with_size (leaf [ Instruction.L2I ;
                              Instruction.IFEQ (Utils.s2 1) ]) (* invalid offset *)
          end else
            with_size (leaf [ Instruction.L2I ;
                              Instruction.IFNE (check_jump (start_offset - if_offset)) ]) in
        curr_ofs := !curr_ofs + sign_sz + body_sz + cond_sz + jump_sz;
        curr_body :=
          !curr_body
          @ [ sign_instrs;
              body_instrs;
              cond_instrs;
              jump_instrs ]
      done;
      let end_offset = !curr_ofs in
      ifeq_offsets := List.rev !ifeq_offsets;
      let curr_body =
        Instrtree.map
          (function
            | Instruction.IFEQ ofs when (ofs :> int) = 1 ->
                let if_offset = match !ifeq_offsets with
                | hd :: tl -> ifeq_offsets := tl; hd
                | [] -> assert false in
                Instruction.IFEQ (check_jump (end_offset - if_offset))
            | x -> x)
          (node !curr_body) in
      node
        [ fisrtcond_instrs ;
          leaf [ Instruction.L2I ;
                 Instruction.IFEQ (check_jump (end_offset - (ofs + firstcond_sz + 1))) ] ;
          curr_body ]
  | Mwhile (cond, body, None) ->
      (* compilation scheme:
           goto lbl
         start:
           <test signals>
           <body>
         lbl:
           <cond> # supposed to push a long
           l2i
           ifne start *)
      let sign_instrs, sign_sz =
        with_size (compile_signals ()) in
      let body_instrs, body_sz =
        with_size (compile_expression false (ofs + goto_size + sign_sz) body) in
      let cond_instrs, cond_sz =
        with_size (compile_expression false (ofs + goto_size + sign_sz + body_sz) cond) in
      node
        [ leaf [ Instruction.GOTO (check_jump (goto_size + sign_sz + body_sz)) ] ;
          sign_instrs ;
          body_instrs ;
          cond_instrs ;
          leaf [ Instruction.L2I ;
                 Instruction.IFNE (check_jump ~-(1 + cond_sz + body_sz + sign_sz)) ] ]
  | Mfor (idx, dir, bound, body, lii) ->
      let times = match lii with Some x -> x | None -> 1 in
      (* compilation scheme:
           push idx # supposed to be a long
           push bound # supposed to be a long
           lcmp
           if<gt,lt> end
         start:
           <sign_1>
           <body_1>
           push idx # supposed to be a long
           update idx
           push bound # supposed to be a long
           lcmp
           if<gt,lt> end
           ...
           <sign_n>
           <body_n>
           push idx # supposed to be a long
           update idx
           push bound # supposed to be a long
           lcmp
           if<le,ge> start
         end: *)
      let push_idx_instrs, push_idx_sz = with_size (lload idx) in
      let push_bound_instrs, push_bound_sz = with_size (lload bound) in
      let update_idx_instrs, update_idx_sz =
        let op = if dir = Upto then Instruction.LADD else Instruction.LSUB in
        with_size (node [ leaf [ Instruction.ICONST_2 ;
                                 Instruction.I2L ;
                                 op ;
                                 Instruction.DUP2 ] ;
                          lstore idx ]) in
      let start_offset = ofs + push_idx_sz + push_bound_sz + 1 + if_size in
      let curr_ofs = ref start_offset in
      let curr_body = ref [] in
      let ifxe_offsets = ref [] in
      for i = 1 to times do
        let sign_instrs, sign_sz = with_size (compile_signals ()) in
        let body_instrs, body_sz =
          with_size (compile_expression false (!curr_ofs + sign_sz) body) in
        let if_offset =
          !curr_ofs + sign_sz + body_sz + push_idx_sz + update_idx_sz + push_bound_sz + 1 in
        let jump_instrs, jump_sz =
          if i < times then begin
            (* store the current offset, to be patched later *)
            ifxe_offsets := if_offset :: !ifxe_offsets;
            with_size
              (leaf [ (if dir = Upto then
                Instruction.IFGT (Utils.s2 1)
              else
                Instruction.IFLT (Utils.s2 1)) ]) (* invalid offset *)
          end else
            let ofs = check_jump (start_offset - if_offset) in
            with_size
              (leaf [ (if dir = Upto then
                Instruction.IFLE ofs
              else
                Instruction.IFGE ofs) ]) in
        curr_ofs := if_offset + jump_sz;
        curr_body :=
          !curr_body
          @ [ sign_instrs;
              body_instrs;
              push_idx_instrs;
              update_idx_instrs;
              push_bound_instrs;
              leaf [ Instruction.LCMP ];
              jump_instrs ]
      done;
      let end_offset = !curr_ofs in
      ifxe_offsets := List.rev !ifxe_offsets;
      let get_next_offset () =
        match !ifxe_offsets with
        | hd :: tl -> ifxe_offsets := tl; hd
        | [] -> assert false in
      let curr_body =
        Instrtree.map
          (function
            | Instruction.IFGT ofs when (ofs :> int) = 1 ->
                let if_offset = get_next_offset () in
                Instruction.IFGT (check_jump (end_offset - if_offset))
            | Instruction.IFLT ofs when (ofs :> int) = 1 ->
                let if_offset = get_next_offset () in
                Instruction.IFLT (check_jump (end_offset - if_offset))
            | x -> x)
          (node !curr_body) in
      let offset_first_if = ofs + push_idx_sz + push_bound_sz + 1 in
      node
        [ push_idx_instrs ;
          push_bound_instrs ;
          leaf [ Instruction.LCMP ;
                 (if dir = Upto then
                   Instruction.IFGT (check_jump (end_offset - offset_first_if))
                 else
                   Instruction.IFLT (check_jump (end_offset - offset_first_if))) ] ;
          curr_body ]
  | Munit ->
      push_int 0
  | Mboxedunit ->
      field_Value_UNIT
  | Mpop Boxed_value ->
      leaf [ Instruction.POP ]
  | Mpop Tagged_int ->
      leaf [ Instruction.POP2 ]
  | Mpop Normalized_int ->
      leaf [ Instruction.POP2 ]
  | Mpop Unboxed_int ->
      leaf [ Instruction.POP2 ]
  | Mpop Unboxed_int32 ->
      leaf [ Instruction.POP ]
  | Mpop Unboxed_int64 ->
      leaf [ Instruction.POP2 ]
  | Mpop Unboxed_nativeint ->
      leaf [ Instruction.POP2 ]
  | Mpop Unboxed_float ->
      leaf [ Instruction.POP2 ]
  | Mpop Unboxed_instance _ ->
      leaf [ Instruction.POP ]
  | Mpop Unboxed_java_array _ ->
      leaf [ Instruction.POP ]
  | Mnop ->
      leaf []
  | Mtrywith (body, Some idx, handler) ->
      (* compilation scheme:
           <body>
           goto end:
           invokestatic Fail.asValue(Throwable):Value
           astore idx
           <handler>
         end: *)
      let body_instrs, body_sz =
        with_size (compile_expression false ofs body) in
      let store_instrs, store_sz =
        with_size (astore idx) in
      let handler_instrs, handler_sz =
        with_size (compile_expression is_tail (ofs + body_sz + goto_size + meth_size + store_sz) handler) in
      State.add_exception
        (Utils.u2 ofs)
        (Utils.u2 (ofs + body_sz))
        (Utils.u2 (ofs + body_sz + goto_size));
      node
        [ body_instrs ;
          leaf [ Instruction.GOTO (check_jump (goto_size + meth_size + store_sz + handler_sz)) ] ;
          meth_asValue ;
          store_instrs ;
          handler_instrs ]
  | Mtrywith (body, None, handler) ->
      (* compilation scheme:
           <body>
           goto end:
           <handler>
         end: *)
      let body_instrs, body_sz =
        with_size (compile_expression false ofs body) in
      let handler_instrs, handler_sz =
        with_size (compile_expression is_tail (ofs + body_sz + goto_size + 1) handler) in
      State.add_exception
        (Utils.u2 ofs)
        (Utils.u2 (ofs + body_sz))
        (Utils.u2 (ofs + body_sz + goto_size));
      node
        [ body_instrs ;
          leaf [ Instruction.GOTO (check_jump (goto_size + 1 + handler_sz)) ;
                 Instruction.POP ] ;
          handler_instrs ]
  | Mstaticfail (nfail, args) ->
      (* compilation scheme:
           <args>
           goto_w catch_offset *)
      let args_instrs, args_sz =
        with_size (compile_expression_list ofs args) in
      let curr_ofs = ofs + args_sz in
      let catch_ofs_ref = State.get_catch_offset nfail in
      node
        [ args_instrs ;
          Instrtree.suspended
            5
            (lazy begin
              let jump = !catch_ofs_ref - curr_ofs in
              [ Instruction.GOTO_W (Utils.s4 (Int32.of_int jump)) ]
            end) ]
  | Mstaticcatch (nfail, body, handler) ->
      begin try
        compile_staticcatch false is_tail ofs nfail body handler
      with _ ->
        compile_staticcatch true is_tail ofs nfail body handler
      end
  | Mswitch (arg, consts, blocks, actions, default) ->
      begin try
        compile_switch false is_tail ofs arg consts blocks actions default
      with _ ->
        compile_switch true is_tail ofs arg consts blocks actions default
      end
  | Mconvert (src, dst, expr) ->
      let rec optimize_conversion src expr =
        match expr with
        | Mconvert (s, _, e) -> optimize_conversion s e
        | _ -> src, expr in
      let src, expr = optimize_conversion src expr in
      compile_conv ofs src dst expr
  | Minit (Some idx) ->
      node
        [ push_int32 (Int32.of_int idx) ;
          meth_initGlobalBegin ]
  | Minit None ->
      meth_initGlobalEnd

and compile_conv ofs src dst expr =
  match src, dst, expr with
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 0))) ->
      field_Value_ZERO
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 1))) ->
      field_Value_ONE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 2))) ->
      field_Value_TWO
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 3))) ->
      field_Value_THREE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 4))) ->
      field_Value_FOUR
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 5))) ->
      field_Value_FIVE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 6))) ->
      field_Value_SIX
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 7))) ->
      field_Value_SEVEN
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int 8))) ->
      field_Value_EIGHT
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int (-1)))) ->
      field_Value_MINUS_ONE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int (-2)))) ->
      field_Value_MINUS_TWO
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 0L ->
      field_Value_ZERO
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 1L ->
      field_Value_ONE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 2L ->
      field_Value_TWO
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 3L ->
      field_Value_THREE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 4L ->
      field_Value_FOUR
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 5L ->
      field_Value_FIVE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 6L ->
      field_Value_SIX
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 7L ->
      field_Value_SEVEN
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = 8L ->
      field_Value_EIGHT
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = (-1L) ->
      field_Value_MINUS_ONE
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) when Targetint.to_int64 x = (-2L) ->
      field_Value_MINUS_TWO
  | (Normalized_int | Unboxed_int),
    Tagged_int,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int x))) ->
      if (x <= max_int / 2) && (x >= min_int / 2) then begin
        (* no overflow *)
        let x = (x lsl 1) lor 1 in
        let c = Const_base (Const_int x) in
        Bytecodegen_constants.push_structured_constant c
      end else begin
        (* overflow *)
        let x = Int64.of_int x in
        let x = Int64.logor (Int64.shift_left x 1) Int64.one in
        Bytecodeutils.push_int64 x
      end
  | (Normalized_int | Unboxed_int),
    Tagged_int,
    Mconst (Jlambda.Const_targetint x) ->
      let x = Targetint.to_int64 x in
      let x = Int64.logor (Int64.shift_left x 1) Int64.one in
      Bytecodeutils.push_int64 x
  | (Normalized_int | Unboxed_int),
    Tagged_int,
    Mconst (Jlambda.Lambda_const (Const_base (Const_char x))) ->
      let x = Char.code x in
      let x = (x lsl 1) lor 1 in
      (* Obj.magic is used insted of Char.chr because, due to tagging, the
         actual value can be outside of character bounds *)
      let c = Const_base (Const_char (Obj.magic x)) in
      Bytecodegen_constants.push_structured_constant c
  | Tagged_int,
    (Normalized_int | Unboxed_int),
    Mconst (Jlambda.Lambda_const (Const_base (Const_int x))) ->
      let x = x asr 1 in
      let c = Const_base (Const_int x) in
      Bytecodegen_constants.push_structured_constant c
  | Tagged_int,
    (Normalized_int | Unboxed_int),
    Mconst (Jlambda.Const_targetint x) ->
      let x = Int64.shift_right (Targetint.to_int64 x) 1 in
      Bytecodeutils.push_int64 x
  | Tagged_int,
    (Normalized_int | Unboxed_int),
    Mconst (Jlambda.Lambda_const (Const_base (Const_char x))) ->
      let x = Char.code x in
      let x = x asr 1 in
      (* Obj.magic is used insted of Char.chr because, due to tagging, the
         actual value can be outside of character bounds *)
      let c = Const_base (Const_char (Obj.magic x)) in
      Bytecodegen_constants.push_structured_constant c
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int x))) ->
      Bytecodegen_constants.push_int (Int64.of_int x)
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Const_targetint x) ->
      Bytecodegen_constants.push_int (Targetint.to_int64 x)
  | (Normalized_int | Unboxed_int),
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_char x))) ->
      Bytecodegen_constants.push_int (Int64.of_int (Char.code x))
  | Unboxed_float,
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_float x))) ->
      Bytecodegen_constants.push_float (float_of_string x)
  | Unboxed_int32,
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int32 x))) ->
      Bytecodegen_constants.push_int32 x
  | Unboxed_int64,
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_int64 x))) ->
      Bytecodegen_constants.push_int64 x
  | Unboxed_nativeint,
    Boxed_value,
    Mconst (Jlambda.Lambda_const (Const_base (Const_nativeint x))) ->
      Bytecodegen_constants.push_nativeint x
  | _ ->
      node
        [ compile_expression false ofs expr ;
          compile_conversion src dst ]

and compile_staticcatch wide is_tail ofs nfail body handler =
  (* compilation scheme:
       <body>
       goto[_w] end
       <handler>
     end: *)
  let goto_size = if wide then 5 else 3 in
  let catch_offset = ref 0 in
  State.add_catch nfail catch_offset;
  let body_instrs, body_sz =
    with_size (compile_expression is_tail ofs body) in
  catch_offset := ofs + body_sz + goto_size;
  let handler_instrs, handler_sz =
    with_size (compile_expression is_tail (ofs + body_sz + goto_size) handler) in
  let ofs_goto = ofs + body_sz in
  let ofs_end = ofs + body_sz + goto_size + handler_sz in
  let jump = ofs_end - ofs_goto in
  node
    [ body_instrs ;
      leaf [ if wide then
        Instruction.GOTO_W (Utils.s4 (Int32.of_int jump))
      else
        Instruction.GOTO (check_jump jump) ] ;
      handler_instrs ]

and compile_switch wide is_tail ofs arg consts blocks actions default =
  (* compilation scheme:
       <arg>
       tableswitch def -length(blocks) length(consts)-1
         -length(blocks)   => actions[idx(blocks[0])]
         -length(blocks)+1 => actions[idx(blocks[1])]
         ...
         0                 => actions[idx(consts[0])]
         length(consts)-1  => actions[idx(consts[length(consts)-1])]
     action_0:
       <actions[0]>
       goto[_w] end
     ...
     action_n:
       <actions[n]>
       goto[_w] end
     def:
       <default>
     end: *)
  let goto_size = if wide then 5 else 3 in
  let array_map f a = (* Array.map does not guarantee evaluation order *)
    let len = Array.length a in
    if len > 0 then
      let res = Array.make len (f a.(0)) in
      for i = 1 to pred len do
        res.(i) <- f a.(i)
      done;
      res
    else
      [| |] in
  let total_len =
    (Array.length consts) + (Array.length blocks) in
  let get_tag =
    if (Array.length blocks) > 0 then
      meth_switchTag
    else
      leaf [ Instruction.L2I ] in
  let arg_instrs, arg_sz =
    with_size (node [ compile_expression false ofs arg ; get_tag ]) in
  let switch_ofs = ofs + arg_sz in
  let padding = 3 - (switch_ofs mod 4) in
  let switch_sz = 1 + padding + 4 + 4 + 4 + (4 * total_len) in
  let o = ref (switch_ofs + switch_sz) in
  let actions =
    array_map
      (fun act ->
        let act_instrs, act_sz = with_size (compile_expression is_tail !o act) in
        let res = act_instrs, !o, !o + act_sz in
        o := !o + act_sz + goto_size;
        res)
      actions in
  let default_instrs, default_sz = with_size (compile_expression false !o default) in
  o := !o + default_sz;
  let all = (List.rev (Array.to_list blocks)) @ (Array.to_list consts) in
  node
    [ arg_instrs ;
      Instrtree.leaf
        ~ofs:switch_ofs
        [ Instruction.TABLESWITCH
            (Utils.s4 (Int32.of_int (!o - switch_ofs - default_sz)),
             Utils.s4 (Int32.neg (Int32.of_int (Array.length blocks))),
             Utils.s4 (Int32.of_int (pred (Array.length consts))),
             List.map
               (fun idx ->
                 let _, o, _ = actions.(idx) in
                 Utils.s4 (Int32.of_int (o - switch_ofs)))
               all) ] ;
      node
        (Array.to_list
           (Array.map
              (fun (instrs, _, ofs) ->
                node
                  [ instrs ;
                    leaf [ if wide then
                      Instruction.GOTO_W (Utils.s4 (Int32.of_int (!o - ofs)))
                    else
                      Instruction.GOTO (check_jump (!o - ofs)) ] ])
              actions)) ;
      default_instrs ]

and compile_first_closure cls func arity java_arity =
  if arity = 1 then
    node
      [ push_method_handle cls (normalize_function_name func) java_arity ;
        meth_setClosure1 ],
    2
  else
    node
      [ push_method_handle cls (normalize_function_name func) java_arity ;
        push_int32 (Int32.of_int arity) ;
        meth_setClosureN ],
    3

and compile_other_closures next = function
  | [] -> next, leaf []
  | others ->
      let index, instrs =
        List.fold_left
          (fun (index, instrs)
              { mfun_label = { Jlambda.fl_class; fl_method };
                mfun_ocaml_arity;
                mfun_java_arity } ->
            if mfun_ocaml_arity = 1 then
              let res =
                node
                  [ push_method_handle
                      fl_class
                      (normalize_function_name fl_method)
                      mfun_java_arity ;
                    push_int index ;
                    meth_setInfix1 ] in
              index + 3, res :: instrs
            else
              let res =
                node
                  [ push_method_handle
                      fl_class
                      (normalize_function_name fl_method)
                      mfun_java_arity ;
                    push_int index ;
                    push_int32 (Int32.of_int mfun_ocaml_arity) ;
                    meth_setInfixN ] in
              index + 4, res :: instrs)
          (next, [])
          others in
      index, (node (List.rev instrs))

and compile_expression_list ofs l =
  l
  |> List.fold_left
      (fun (o, acc) elem ->
        let res, sz = with_size (compile_expression false o elem) in
        o + sz, res :: acc)
      (ofs, [])
  |> snd
  |> List.rev
  |> node

and compile_expression_list_to_locals ofs l arg_kinds =
  let _, _, exprs, stores =
    List.fold_left2
      (fun (ofs, idx, exprs, stores) expr kind ->
        let expr_instrs, expr_sz =
          with_size (compile_expression false ofs expr) in
        let store_instrs, idx_sz =
          match kind with
          | Boxed_value -> astore idx, 1
          | Tagged_int -> lstore idx, 2
          | Normalized_int -> lstore idx, 2
          | Unboxed_int -> lstore idx, 2
          | Unboxed_int32 -> istore idx, 1
          | Unboxed_int64 -> lstore idx, 2
          | Unboxed_nativeint -> lstore idx, 2
          | Unboxed_float -> dstore idx, 2
          | Unboxed_instance _ -> astore idx, 1
          | Unboxed_java_array _ -> astore idx, 1 in
        (ofs + expr_sz, idx + idx_sz, expr_instrs :: exprs, store_instrs :: stores))
      (ofs, 0, [], [])
      l
      arg_kinds in
  node ((List.rev exprs) @ stores)

and compile_expression_list_to_value ofs index l =
  let dup_size = 1 in
  let _, _, instrs =
    List.fold_left
      (fun (ofs, index, instrs) expr ->
        let dup = leaf [ Instruction.DUP ] in
        let expr_instrs = compile_expression false (ofs + dup_size) expr in
        let set_instrs = set_value index in
        let res_instrs, res_sz = with_size (node [ dup ; expr_instrs; set_instrs ]) in
        (ofs + res_sz, succ index, res_instrs :: instrs))
      (ofs, index, [])
      l in
  node (List.rev instrs)

and compile_expression_list_to_array ofs l =
  let dup_size = 1 in
  let size_instrs, size_sz =
    with_size (push_int32 (Int32.of_int (List.length l))) in
  let array_instrs, array_sz =
    with_size (leaf [ Instruction.ANEWARRAY (`Class_or_interface class_Value) ]) in
  let _, _, instrs =
    List.fold_left
      (fun (ofs, idx, instrs) expr ->
        let idx_instrs, idx_sz = with_size (push_int32 idx) in
        let expr_instrs = compile_expression false (ofs + dup_size + idx_sz) expr in
        let res_instrs, res_sz =
          with_size (node
                       [ leaf [ Instruction.DUP ] ;
                         idx_instrs ;
                         expr_instrs ;
                         leaf [ Instruction.AASTORE ] ]) in
        (ofs + res_sz, Int32.succ idx, res_instrs :: instrs))
      (ofs + size_sz + array_sz, 0l, []) 
      l in
  node
    [ size_instrs ;
      array_instrs ;
      node (List.rev instrs) ]

let compile_fields ppf =
  let current_class = make_class (State.current_class ()) in
  let fields =
    Field.([ { flags = [ `Private ; `Static ; `Final ];
               name = Name.make_for_field (UTF8.of_string "CONSTANTS");
               descriptor = `Class class_ThreadLocal;
               attributes = []; } ;
             { flags = [ `Public ; `Static ; `Final ];
               name = Name.make_for_field (UTF8.of_string "GLOBALS");
               descriptor = `Class class_ThreadLocal;
               attributes = []; } ])
    @ List.map
        (fun (name, _) ->
          Field.({ flags = [ `Private ; `Static ; `Final ];
                   name;
                   descriptor = `Class class_Value;
                   attributes = []; }))
        !constant_closures in
  let consts =
    node [ leaf [ Instruction.LDC_W (`Class_or_interface current_class) ] ;
           meth_constantsStorage ;
           leaf [ Instruction.PUTSTATIC
                    (current_class,
                     Name.make_for_field (UTF8.of_string "CONSTANTS"),
                     (`Class class_ThreadLocal)) ] ] in
  let globals =
    node [ leaf [ Instruction.LDC_W (`String (UTF8.of_string (State.current_module ()))) ] ;
           meth_globalStorage ;
           leaf [ Instruction.PUTSTATIC
                    (current_class,
                     Name.make_for_field (UTF8.of_string "GLOBALS"),
                     (`Class class_ThreadLocal)) ] ] in
  let long_fields, long_inits = Bytecodegen_constants.get_fields_and_inits () in
  let closures =
    node
      (List.map
         (fun (name, instrs) ->
           node [ instrs ;
                  leaf [ Instruction.PUTSTATIC (make_class (State.current_class ()),
                                                name,
                                                `Class class_Value) ] ])
         !constant_closures) in
  (* long constants can be used in "createConstants", and should hence be
     initialized before. *)
  let instrs =
    [ Instruction.INVOKESTATIC (class_AbstractNativeRunner,
                                make_method "sharedConstantsBegin",
                                ([], `Void)) ]
    @ long_inits
    @ [ Instruction.INVOKESTATIC (class_AbstractNativeRunner,
                                  make_method "sharedConstantsEnd",
                                  ([], `Void)) ]
    @ (Instrtree.flatten consts)
    @ (Instrtree.flatten globals)
    @ (Instrtree.flatten closures)
    @ [ Instruction.RETURN ] in
  if !Jclflags.dump_bytecode || !Jclflags.dump_optbytecode then
    Printbytecode.bytecode ppf "[optimized] static block:" instrs [];
  let code = Attribute.({ max_stack = Utils.u2 8;
                          max_locals = Utils.u2 0;
                          code = instrs;
                          exception_table = [];
                          attributes = [] }) in
  Method.(Initializer { init_flags = [`Static];
                        init_attributes = [`Code code]}),
  fields @ long_fields

let type_of_repr = function
  | LR_value | LR_string | LR_exn
  | LR_array _ | LR_list _ | LR_option _ | LR_lazy _ -> `Class class_Value
  | LR_int | LR_char | LR_bool -> `Long
  | LR_float -> `Double
  | LR_unit -> assert false
  | LR_nativeint -> `Long
  | LR_int32 -> `Int
  | LR_int64 -> `Long
  | LR_java_instance cn | LR_java_extends cn -> `Class (make_class cn)
  | (LR_java_boolean_array
  | LR_java_byte_array
  | LR_java_char_array
  | LR_java_double_array
  | LR_java_float_array
  | LR_java_int_array
  | LR_java_long_array
  | LR_java_reference_array _
  | LR_java_short_array) as repr ->
      (Jlambda.unconvert_array_type (array_type_of_repr repr) :> Descriptor.for_parameter)
  | LR_none -> assert false

let compile_function ppf fd =
  State.reset_state
    ~file:(String.uncapitalize (Jcompilenv.current_unit_name ()) ^ ".ml")
    ~clas:(!Jclflags.java_package ^ "." ^ (Jcompilenv.current_unit_name ()))
    ~func:fd.fun_name;
  let _, identifiers, parameters =
    List.fold_left2
      (fun (acc_idx, acc_ids, acc_params) id elem ->
        if elem <> LR_unit then
          let typ = type_of_repr elem in
          let sz =
            match typ with
            | `Long | `Double -> 2
            | `Array _ | `Float | `Class _ | `Byte
            | `Char | `Boolean | `Int | `Short -> 1 in
          (acc_idx + sz, (acc_idx, id) :: acc_ids, typ :: acc_params)
        else
          (acc_idx, acc_ids, acc_params))
      (0, [], [])
      fd.fun_args
      fd.fun_params in
  let identifiers = List.rev identifiers in
  let parameters = List.rev parameters in
  let tree = compile_expression true 0 fd.fun_body in
  let instrs = Instrtree.flatten tree in
  let exception_table = State.compile_exception_table () in
  let instrs =
    if (size tree) >= 65535 then begin
      let contains_jump =
        List.exists
          (fun instr ->
            match ControlFlow.for_instruction instr with
            | ControlFlow.Unconditional_jump _
            | ControlFlow.Conditional_jump _
            | ControlFlow.Switch_jump _-> true
            | ControlFlow.Next_instruction
            | ControlFlow.Called_method
            | ControlFlow.Calling_method _ 
            | ControlFlow.Exception_handler -> false)
          instrs in
      if (exception_table = []) && (not contains_jump) then
        Bytecodegen_size.shrink instrs
      else
        instrs
    end else
      instrs in
  if Instruction.size_of_list 0 instrs >= 65535 then begin
    Printbytecode.bytecode
      ppf
      (Printf.sprintf "bytecode for '%s':" fd.fun_name)
      instrs
      exception_table;
    raise (Error (Method_too_long (fd.fun_name, true, size tree)))
  end;
  let return_instr, return_type =
    if fd.fun_name = "entry" then
      [ Instruction.ARETURN ], `Class class_Value
    else if fd.fun_return = LR_unit then
      [ Instruction.POP; Instruction.RETURN ], `Void
    else
      let typ = type_of_repr fd.fun_return in
      let instr =
        match typ with
        | `Boolean | `Byte | `Char | `Float | `Short -> assert false
        | `Double  -> [ Instruction.DRETURN ]
        | `Int     -> [ Instruction.IRETURN ]
        | `Long    -> [ Instruction.LRETURN ]
        | `Class _ -> [ Instruction.ARETURN ]
        | `Array _ -> [ Instruction.ARETURN ] in
      instr, (typ :> Descriptor.java_type) in
  let instrs = instrs @ return_instr in
  if !Jclflags.dump_bytecode then
    Printbytecode.bytecode
      ppf
      (Printf.sprintf "bytecode for '%s':" fd.fun_name)
      instrs
      exception_table;
  let code, line_numbers, exception_table, graph =
    try
      let mapper =
        ControlFlow.line_number_table_mapper
          (State.compile_line_numbers ()) in
      let graph =
        ControlFlow.graph_of_instructions
          ~line_mapper:mapper
          instrs
          exception_table in
      let evaluation = PartialEvaluation.make_of_parameters false parameters in
      graph
      |> Peephole.optimize_graph ~rules:peephole_rules
      |> Code.optimize_jumps
      |> Code.optimize_switches
      |> Code.optimize_partial_evaluation evaluation
      |> Code.optimize_partial_evaluation evaluation
      |> Code.remove_dead_code
      |> Code.flatten_graph
    with e ->
      let err = Unable_to_optimize (fd.fun_name, Printexc.to_string e) in
      raise (Error err) in
  if !Jclflags.dump_optbytecode then
    Printbytecode.bytecode
      ppf
      (Printf.sprintf "optimized bytecode for '%s':" fd.fun_name)
      code
      exception_table;
  let init_state = StackState.make_of_parameters None parameters in
  let max_stack, max_locals, stack_map_frame =
    try
      let current_class = make_class (State.current_class ()) in
      Code.compute_stack_infos current_class (Jutils.get_unifier ()) graph init_state
    with
    | StackState.Exception sse ->
        let err = Unable_to_compute_stack (fd.fun_name, StackState.string_of_error sse) in
        raise (Error err)
    | e ->
        let err = Unable_to_compute_stack (fd.fun_name, Printexc.to_string e) in
        raise (Error err) in
  let code_length = Utils.u2 (Instruction.size_of_list 0 code) in
  let attributes =
    (if stack_map_frame <> [] then
      [ `StackMapTable stack_map_frame ]
    else
      [])
    @ (if line_numbers <> [] then
      [ `LineNumberTable (List.sort compare line_numbers) ]
    else
      [])
    @ (if !Clflags.debug then
      [ `LocalVariableTable
          (List.map2
             (fun (idx, id) typ ->
               Attribute.({ local_start = Utils.u2 0;
                            local_length = code_length;
                            local_name = UTF8.of_string (Ident.name id);
                            local_descriptor = typ;
                            local_index = Utils.u2 idx; }))
             identifiers
             parameters) ]
    else
      []) in
  let code =
    Attribute.({ max_stack; max_locals; code; exception_table; attributes }) in
  Method.(Regular { flags = [`Public; `Static];
                    name = make_method fd.fun_name;
                    descriptor = (parameters, return_type);
                    attributes = [`Code code;
                                  `Exceptions [class_FailException]]; })

let compile_surrogate fd =
  let _, call_parameters, max_stack, instrs =
    List.fold_right
      (fun elem (idx, acc_params, acc_stack_size, acc_instrs) ->
        let kind = kind_of_repr elem in
        let size = size_of_value_kind kind in
        let acc_params, instrs =
          if elem <> LR_unit then
            (type_of_repr elem) :: acc_params,
            node
              [ aload idx ;
                compile_conversion Boxed_value kind ]
          else
            acc_params, leaf [] in
        (pred idx, acc_params, acc_stack_size + size, instrs :: acc_instrs))
      fd.fun_params
      (List.length fd.fun_params - 1, [], 2, []) in
  let ret_type, ret_conv =
    if fd.fun_return <> LR_unit then
      (type_of_repr fd.fun_return :> Descriptor.java_type),
      compile_conversion (kind_of_repr fd.fun_return) Boxed_value
    else
      `Void,
      field_Value_UNIT in
  let instrs =
    (instrs
     @ [ leaf
           [ Instruction.INVOKESTATIC
               (make_class (State.current_class ()),
                make_method fd.fun_name,
                (call_parameters, ret_type)) ] ;
         ret_conv ;
         leaf [ Instruction.ARETURN ] ])
    |> node
    |> Instrtree.flatten in
  let code = Attribute.({ max_stack = Utils.u2 max_stack;
                          max_locals = Utils.u2 (List.length fd.fun_args);
                          code = instrs;
                          exception_table = [];
                          attributes = [] }) in
  Method.(Regular { flags = [`Public; `Static];
                    name = make_method fd.fun_name;
                    descriptor = (repeat_parameters (List.length fd.fun_args),
                                  `Class class_Value);
                    attributes = [`Code code;
                                  `Exceptions [class_FailException] ]; })

let compile_constants ppf f =
  let consts_code = f () in
  if (size consts_code) < 65536 then begin
    let instrs = Instrtree.flatten consts_code in
    if !Jclflags.dump_bytecode then
      Printbytecode.bytecode ppf "bytecode for constants:" instrs [];
    let code, _, _, graph =
      ControlFlow.graph_of_instructions instrs []
      |> Peephole.optimize_graph ~rules:peephole_rules
      |> Code.optimize_jumps
      |> Code.remove_dead_code
      |> Code.flatten_graph in
    let init_state =
      StackState.make_of_parameters None [] in
    let current_class = make_class (State.current_class ()) in
    let max_stack, max_locals, stack_map_frame =
      Code.compute_stack_infos current_class (Jutils.get_unifier ()) graph init_state in
    let attributes =
      if stack_map_frame <> [] then
        [ `StackMapTable stack_map_frame ]
      else
        [] in
    let code =
      Attribute.({ max_stack; max_locals; code; exception_table = []; attributes }) in
    let const_class =
      Bytecodegen_constants.const_class_of_curr_class (State.current_class ()) in
    Method.(Regular { flags = [`Public; `Static];
                      name = make_method "createConstants";
                      descriptor = ([], `Class const_class);
                      attributes = [`Code code] })
  end else
    raise Not_found

let compile_constants_to_method ppf =
  try
    compile_constants ppf Bytecodegen_constants.init_class_fields_from_code,
    false
  with _ ->
    compile_constants ppf Bytecodegen_constants.init_class_fields_from_load,
    true

let compile_get_global_method classname =
  let classname = make_class classname in
  let instrs =
    let current_class = make_class (State.current_class ()) in
    node
      [ leaf [ Instruction.GETSTATIC (current_class,
                                      make_field "GLOBALS",
                                      `Class class_ThreadLocal) ] ;
        meth_get_threadlocal ;
        leaf [ Instruction.CHECKCAST (`Class_or_interface classname) ;
               Instruction.ARETURN ] ] in
  let instrs = Instrtree.flatten instrs in
  let code = Attribute.({ max_stack = Utils.u2 1;
                          max_locals = Utils.u2 0;
                          code = instrs;
                          exception_table = [];
                          attributes = [] }) in
  Method.(Regular { flags = [`Public; `Static];
                    name = make_method "getGlobal";
                    descriptor = ([], `Class classname);
                    attributes = [`Code code] })

let compile_class fields methods annots =
  let classname = !Jclflags.java_package ^ "." ^ (Jcompilenv.current_unit_name ()) in
  let source_file =
    String.uncapitalize (Jcompilenv.current_unit_name ()) ^ ".ml" in
  let annotation = class_CompiledModule, [] in
  let cd = { ClassDefinition.access_flags = [`Public; `Final; `Super];
             name = make_class classname;
             extends = Some class_Object;
             implements = [ interface_OCamlJavaModule ];
             fields;
             methods;
             attributes = [ `SourceFile (UTF8.of_string source_file) ;
                            `RuntimeVisibleAnnotations [ annotation ] ;
                            `RuntimeInvisibleAnnotations annots ]; } in
  let buff = ByteBuffer.make_of_size 2048 in
  let os = OutputStream.make_of_buffer buff in
  ClassFile.write (ClassDefinition.encode cd) os;
  OutputStream.close os;
  ByteBuffer.contents buff
