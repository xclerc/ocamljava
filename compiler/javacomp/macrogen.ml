(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (asmcomp/cmmgen.ml in the OCaml source
 * distribution) is Copyright (C) INRIA.
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
open Jlambda
open Macroinstr
open Macrogen_primitives
open Macrogen_global


type error =
  | Special_primitive_string of string

exception Error of error

(* method frames *)

type frame =
    { elements : (value_kind * int) option Ident.tbl;
      next : int; }

let make_frame entry params =
  let vars, repr_list = List.split params in
  let elems, nxt =
    if entry then
      (Ident.add (Ident.create "_dummy_") (Some (Boxed_value, 0)) Ident.empty), 1
    else
      Ident.empty, 0 in
  let elems, nxt =
    List.fold_left2
      (fun (acc, idx) elem r ->
        match r with
        | LR_unit ->
            Ident.add elem None acc, idx
        | _ ->
            let kind = kind_of_repr r in
            let size = size_of_value_kind kind in
            Ident.add elem (Some (kind, idx)) acc, idx + size)
      (elems, nxt)
      vars
      repr_list in
  { elements = elems; next = nxt; }

let add_var kind id frm =
  let sz = size_of_value_kind kind in
  let res = { elements = Ident.add id (Some (kind, frm.next)) frm.elements;
              next = frm.next + sz; } in
  frm.next, res


(* recursive values *)

let fundecls_size fundecls =
  let sz = ref (-1) in
  List.iter
    (fun { arity; label = _; params = _; return = _; body = _; dbg = _ } ->
      sz := !sz + 1 + (if arity = 1 then 2 else 3))
    fundecls;
  !sz

type rhs_kind =
  | RHS_block of int
  | RHS_floatblock of int
  | RHS_nonrec

let rec expr_size env = function
  | Jvar id ->
      begin try
        Ident.find_same id env
      with Not_found ->
        RHS_nonrec
      end
  | Jclosure (fundecls, clos_vars) ->
      RHS_block (fundecls_size fundecls + List.length clos_vars)
  | Jlet (id, exp, body) ->
      expr_size (Ident.add id (expr_size env exp) env) body
  | Jletrec (_, body) ->
      expr_size env body
  | Jprim (Pmakeblock _, args, _) ->
      RHS_block (List.length args)
  | Jprim (Pmakearray (Paddrarray | Pintarray), args, _) ->
      RHS_block (List.length args)
  | Jprim (Pmakearray (Pfloatarray), args, _) ->
      RHS_floatblock (List.length args)
  | Jprim (Pduprecord (Types.Record_regular, sz), _, _) ->
      RHS_block sz
  | Jprim (Pduprecord (Types.Record_float, sz), _, _) ->
      RHS_floatblock sz
  | Jsequence (_, j) ->
      expr_size env j
  | _ ->
      RHS_nonrec

(* translation from jlambda to macroinstr *)

let return_unit k x =
  match k with
  | Boxed_value -> Msequence (x, Mboxedunit)
  | _ -> Msequence (x, Munit)

let rec remove_unit = function
  | Munit -> Mnop
  | Mboxedunit -> Mnop
  | Msequence (m, Munit) -> m
  | Msequence (m, Mboxedunit) -> m
  | Msequence (m1, m2) -> Msequence (m1, remove_unit m2)
  | Mifthenelse (cond, ifso, ifnot) ->
      Mifthenelse (cond, remove_unit ifso, remove_unit ifnot)
  | Mswitch (arg, index, blocks, cases, default) ->
      Mswitch (arg, index, blocks, Array.map remove_unit cases, remove_unit default)
  | Mstaticcatch (nfail, body, handler) ->
      Mstaticcatch (nfail, remove_unit body, remove_unit handler)
  | Mtrywith (body, exn, handler) ->
      Mtrywith (remove_unit body, exn, remove_unit handler)
  | m -> Msequence (m, (Mpop Boxed_value))

let int_const =
  if Jconfig.ints_are_63_bit_long then
    Normalized_int
  else
    Unboxed_int

let kind_of_constant = function
  | Const_base (Const_int _)       -> int_const
  | Const_base (Const_char _)      -> int_const
  | Const_base (Const_string _)    -> Boxed_value
  | Const_base (Const_float _)     -> Unboxed_float
  | Const_base (Const_int32 _)     -> Unboxed_int32
  | Const_base (Const_int64 _)     -> Unboxed_int64
  | Const_base (Const_nativeint _) -> Unboxed_nativeint
  | Const_pointer _                -> Boxed_value
  | Const_block _                  -> Boxed_value
  | Const_float_array _            -> Boxed_value
  | Const_immstring _              -> Boxed_value

let rec kind_of_jlambda stack_frame = function
  | Jvar id ->
      begin try
        match Ident.find_same id stack_frame.elements with
        | Some (k, _) -> k
        | None -> Boxed_value
      with Not_found ->
        Boxed_value
      end
  | Jconst (Jlambda.Lambda_const c) ->
      kind_of_constant c
  | Jconst (Const_targetint _ ) ->
      int_kind
  | Jconst (Const_null (Some (`Class cn))) ->
      Unboxed_instance cn
  | Jconst (Const_null (Some (`Array arr))) ->
      Unboxed_java_array (`Array arr)
  | Jconst (Const_null (Some (`Boolean | `Byte | `Char | `Double | `Float | `Int | `Long | `Short))) ->
          assert false
  | Jconst (Const_null None) ->
      Unboxed_instance "java.lang.Object"
  | Jconst (Const_javastring _) ->
      Unboxed_instance "java.lang.String"
  | Jdirect_apply (_, _, _, return, _) ->
      kind_of_repr return
  | Jgeneric_apply _ ->
      Boxed_value
  | Jclosure _ ->
      Boxed_value
  | Joffset _ ->
      Boxed_value
  | Jlet (id, e, j) ->
      let k = kind_of_jlambda stack_frame e in
      let _, stack_frame = add_var k id stack_frame in
      kind_of_jlambda stack_frame j
  | Jletrec (b, j) ->
      let stack_frame =
        List.fold_left
          (fun stack_frame (id, e) ->
            let k = kind_of_jlambda stack_frame e in
            snd (add_var k id stack_frame))
          stack_frame
          b in
      kind_of_jlambda stack_frame j
  | Jprim (Pignore, [_arg], _) ->
      (* for Pignore, the representation of argument may vary *)
      Boxed_value
  | Jprim (p, args, _) ->
      snd (signature_of_primitive p args)
  | Jjavaprim (p, _, _) ->
      snd (signature_of_java_primitive p)
  | Jswitch (_, sw) ->
      if Array.length sw.js_actions > 0 then
        Array.fold_left
          (fun acc elem ->
            combine_kinds acc (kind_of_jlambda stack_frame elem))
          (kind_of_jlambda stack_frame sw.js_actions.(0))
          sw.js_actions
      else
        Boxed_value
  | Jstaticfail _ ->
      Boxed_value
  | Jstaticcatch (_, _, j, j') ->
      combine_kinds
        (kind_of_jlambda stack_frame j)
        (kind_of_jlambda stack_frame j')
  | Jtrywith (j, _, j') ->
      combine_kinds
        (kind_of_jlambda stack_frame j)
        (kind_of_jlambda stack_frame j')
  | Jifthenelse (_, j, j') ->
      combine_kinds
        (kind_of_jlambda stack_frame j)
        (kind_of_jlambda stack_frame j')
  | Jsequence (_, j) ->
      kind_of_jlambda stack_frame j
  | Jwhile _ ->
      Boxed_value
  | Jfor _ ->
      Boxed_value
  | Jassign _ ->
      Boxed_value
  | Jsend _ ->
      Boxed_value
  | Jnop ->
      Boxed_value

let rec contains_try_with = function
  | Jvar _ ->
      false
  | Jconst _ ->
      false
  | Jdirect_apply (_, l, _, _, _) ->
      list_contains_try_with l
  | Jgeneric_apply (j, l, _) ->
      (contains_try_with j) || (list_contains_try_with l)
  | Jclosure (_, l) ->
      list_contains_try_with l
  | Joffset (j, _) ->
      contains_try_with j
  | Jlet (_, j1, j2) ->
      (contains_try_with j1) || (contains_try_with j2)
  | Jletrec (l, j) ->
      (list_contains_try_with_snd l) || (contains_try_with j)
  | Jprim (_, l, _) ->
      list_contains_try_with l
  | Jjavaprim (_, l, _) ->
      list_contains_try_with l
  | Jswitch (j, sw) ->
      (contains_try_with j) || (array_contains_try_with sw.js_actions)
  | Jstaticfail (_, l) ->
      list_contains_try_with l
  | Jstaticcatch (_, _, j1, j2) ->
      (contains_try_with j1) || (contains_try_with j2)
  | Jtrywith _ ->
      true
  | Jifthenelse (j1, j2, j3) ->
      (contains_try_with j1) || (contains_try_with j2) || (contains_try_with j3)
  | Jsequence (j1, j2) ->
      (contains_try_with j1) || (contains_try_with j2)
  | Jwhile (j1, j2, _) ->
      (contains_try_with j1) || (contains_try_with j2)
  | Jfor (_, j1, j2, _, j3, _) ->
      (contains_try_with j1) || (contains_try_with j2) || (contains_try_with j3)
  | Jassign (_, j) ->
      contains_try_with j
  | Jsend (_, j1, j2, l, _) ->
      (contains_try_with j1) || (contains_try_with j2) || (list_contains_try_with l)
  | Jnop ->
      false
and list_contains_try_with l =
  List.exists
    contains_try_with
    l
and list_contains_try_with_snd l =
  List.exists
    (fun (_, x) -> contains_try_with x)
    l
and array_contains_try_with a =
  let len = Array.length a in
  let i = ref 0 in
  while (!i < len) && not (contains_try_with a.(!i)) do
    incr i
  done;
  !i < len

let convert src dst v =
  if src = dst then v else Mconvert (src, dst, v)

let sequence x y =
  match x, y with
  | Mnop, _ -> y
  | _, Mnop -> x
  | _, _    -> Msequence (x, y)

let default_prim name =
  { Primitive.prim_name = name;
    prim_arity = 0;
    prim_alloc = true;
    prim_native_name = "";
    prim_native_float = false; }

let mk_boxed_int x =
  Mconvert (int_const,
            Boxed_value,
            Mconst (Lambda_const (Const_base (Const_int x))))

let rec transl add_function result_kind stack_frame = function
  | Jvar id ->
      begin match Ident.find_same id stack_frame.elements with
      | Some (kind, index) ->
          Mreadlocal (kind, index)
          |> convert kind result_kind
      | None ->
          Mboxedunit
      end
  | Jconst cst ->
      begin match cst with
      | Jlambda.Lambda_const c ->
          Mconst (Lambda_const c)
          |> convert (kind_of_constant c) result_kind
      | Const_targetint i ->
          Mconst (Const_targetint i)
          |> convert Unboxed_int result_kind
      | Const_null ((Some (`Class cn)) as typ) ->
          Mconst (Const_null typ)
          |> convert (Unboxed_instance cn) result_kind
      | Const_null ((Some (`Array arr)) as typ) ->
          Mconst (Const_null typ)
          |> convert (Unboxed_java_array (`Array arr)) result_kind
      | Const_null (Some (`Boolean | `Byte | `Char | `Double | `Float | `Int | `Long | `Short)) ->
          assert false
      | Const_null None ->
          Mconst (Const_null None)
          |> convert (Unboxed_instance "java.lang.Object") result_kind
      | Const_javastring s ->
          Mconst (Const_javastring s)
          |> convert (Unboxed_instance "java.lang.String") result_kind
      end
  | Jdirect_apply (lbl, args, params, return, dbg) ->
      (* [args] & [kinds] describe the actual parameters to the called
         function, while [discards] represents the evaluation of [unit]
         parameters that are mapped to void. Such parameters have to be
         evaluated as they can contain side effects. *)
      let args, kinds, discards =
        List.fold_left2
          (fun (acc_args, acc_kinds, acc_discards) arg param ->
            match param with
            | LR_unit | LR_none ->
                if Jclosure.is_pure_jlambda arg then
                  acc_args, acc_kinds, acc_discards
                else
                  acc_args, acc_kinds, arg :: acc_discards
            | _ ->
                let param_kind = kind_of_repr param in
                arg :: acc_args, param_kind :: acc_kinds, acc_discards)
          ([], [], [])
          args
          params in
      let args = List.rev args in
      let kinds = List.rev kinds in
      let discards =
        List.fold_left
          (fun acc elem ->
            sequence
              acc
              (sequence
                 (transl add_function Boxed_value stack_frame elem)
                 (Mpop Boxed_value)))
          Mnop
          discards in
      let args, prefix =
        transl_list ~kinds:(Some kinds) add_function stack_frame args in
      let return_kind =
        match return with
        | LR_unit | LR_none -> None
        | _                 -> Some (kind_of_repr return) in
      let call =
        Mcall (lbl, args, kinds, return_kind, dbg)
        |> sequence prefix in
      begin match return_kind with
      | Some k ->
          convert k result_kind call
      | None ->
          sequence call Mboxedunit
      end
      |> sequence discards
  | Jgeneric_apply (clos, args, dbg) ->
      let clos = transl add_function Boxed_value stack_frame clos in
      let idx_clos, stack_frame = add_var Boxed_value (Ident.create "_dummy_") stack_frame in
      let args, prefix = transl_list add_function stack_frame args in
      let res = sequence prefix (Mapply (idx_clos, args, dbg)) in
      convert Boxed_value result_kind
        (sequence
           (Mwritelocal (Boxed_value, idx_clos, clos))
           res)
  | Jclosure (fundecls, clos_vars) ->
      (* note: constant closure are optimized in Bytecodegen *)
      assert (result_kind = Boxed_value);
      List.iter
        (fun { label = { fl_class; fl_method }; arity = _; params; return; body; dbg = _ } ->
          add_function fl_class fl_method params return body)
        fundecls;
      let clos_vars, prefix = transl_list add_function stack_frame clos_vars in
      let funcs =
        List.map
          (fun { label; arity; params; return = _; body = _; dbg = _ } ->
            {  mfun_label = label;
               mfun_ocaml_arity = arity;
               mfun_java_arity = List.length params })
          fundecls in
      Mclosure (fundecls_size fundecls + List.length clos_vars, funcs, clos_vars)
      |> sequence prefix
  | Joffset (j, 0) ->
      transl add_function result_kind stack_frame j
  | Joffset (j, n) ->
      Moffset (transl add_function Boxed_value stack_frame j, n)
      |> convert Boxed_value result_kind
  | Jlet (id, expr, body) ->
      let kind = kind_of_jlambda stack_frame expr in
      let expr = transl add_function kind stack_frame expr in
      let idx_expr, stack_frame = add_var kind id stack_frame in
      Msequence (Mwritelocal (kind, idx_expr, expr),
                 transl add_function result_kind stack_frame body)
  | Jletrec (bindings, body) ->
      (* do not translate body here to be sure to do so with the correct
         stack frame *)
      transl_letrec add_function result_kind stack_frame bindings body
  | Jprim (Pignore, [arg], _dbg) ->
      (* for Pignore, the representation of argument may vary *)
      let kind = kind_of_jlambda stack_frame arg in
      sequence
        (transl add_function kind stack_frame arg)
        (sequence
           (Mpop kind)
           Mboxedunit)
  | Jprim (prim, args, dbg) ->
      begin match simplif_primitive prim with
      | Some prim_name ->
          let desc = Runtimeprimitives.get_description prim_name in
          let fl_class = desc.Runtimeprimitives.primdesc_class in
          let fl_method = desc.Runtimeprimitives.primdesc_method in
          Jdirect_apply ({ fl_class; fl_method },
                         args,
                         desc.Runtimeprimitives.primdesc_parameters,
                         desc.Runtimeprimitives.primdesc_return,
                         dbg)
          |> transl add_function result_kind stack_frame
      | None ->
          let params, return_kind, arg0, args =
            match prim, args with
            | (Pintcomp _), [arg1; arg2] ->
                let kind, code =
                  match arg1, arg2 with
                  | Jconst _, other | other, Jconst _ ->
                      begin match kind_of_jlambda stack_frame other with
                      | Tagged_int -> Tagged_int, 1
                      | Normalized_int -> Normalized_int, 2
                      | Unboxed_int -> Unboxed_int, 3
                      | _ -> Boxed_value, 0
                      end
                  | _ ->
                      match kind_of_jlambda stack_frame arg1,
                            kind_of_jlambda stack_frame arg2 with
                      | Boxed_value, Tagged_int
                      | Tagged_int, Boxed_value
                      | Tagged_int, Tagged_int -> Tagged_int, 1
                      | Normalized_int, (Boxed_value | Tagged_int)
                      | (Boxed_value | Tagged_int), Normalized_int
                      | Normalized_int, Normalized_int -> Normalized_int, 2
                      | Unboxed_int, (Boxed_value | Tagged_int | Normalized_int)
                      | (Boxed_value | Tagged_int | Normalized_int), Unboxed_int
                      | Unboxed_int, Unboxed_int -> Unboxed_int, 3
                      | _ -> Boxed_value, 0 in
                Fixed [kind; kind],
                Normalized_int,
                Some (Mconst (Lambda_const (Const_base (Const_int code)))),
                args
            | _ ->
                let p, r = signature_of_primitive prim args in
                p, r, None, args in
          let kinds =
            match params with
            | Fixed l -> assert (List.length l = List.length args); l
            | Unbounded k -> repeat_kind k (List.length args) in
          let prepend x l =
            match x with
            | Some x -> x :: l
            | None -> l in
          begin match prim with
          | Psequand | Psequor ->
              let args =
                args
                |> List.map2
                    (fun kind expr -> transl add_function kind stack_frame expr)
                    kinds
                |> prepend arg0 in
              Mprim (prim, args, dbg)
              |> convert return_kind result_kind
          | _ ->
              let args, prefix =
                transl_list add_function ~kinds:(Some kinds) stack_frame args in
              Mprim (prim, prepend arg0 args, dbg)
              |> sequence prefix
              |> convert return_kind result_kind
          end
      end
  | Jjavaprim ((Java_synchronized (kind, _)) as jprim, args, dbg) ->
      let idx_lock, stack_frame = add_var (Unboxed_instance "java.lang.Object") (Ident.create "_dummy_") stack_frame in
      let params, return_kind = signature_of_java_primitive jprim in
      let args, prefix = transl_list add_function ~kinds:(Some params) stack_frame args in
      let res = Mjavaprim (Java_synchronized (kind, idx_lock), args, dbg) in
      convert return_kind result_kind (sequence prefix res)
  | Jjavaprim (jprim, args, dbg) ->
      let params, return_kind = signature_of_java_primitive jprim in
      let args, prefix = transl_list add_function ~kinds:(Some params) stack_frame args in
      let res = Mjavaprim (jprim, args, dbg) in
      convert return_kind result_kind (sequence prefix res)
  | Jswitch (arg, sw) ->
      transl_switch add_function result_kind stack_frame arg sw
  | Jstaticfail (nfail, args) ->
      let args, prefix = transl_list add_function stack_frame args in
      Mstaticfail (nfail, args)
      |> sequence prefix
  | Jstaticcatch (nfail, [], body, handler) ->
      make_catch
        nfail
        (transl add_function result_kind stack_frame body)
        (transl add_function result_kind stack_frame handler)
  | Jstaticcatch (nfail, ids, body, handler) ->
      let stack_frame, instrs =
        List.fold_left
          (fun (stack_frame, m) elem ->
            let idx, stack_frame = add_var Boxed_value elem stack_frame in
            (stack_frame, Msequence (Mpoptolocal (Boxed_value, idx), m)))
          (stack_frame, Mnop)
          ids in
      Mstaticcatch (nfail,
                    transl add_function result_kind stack_frame body,
                    (sequence instrs (transl add_function result_kind stack_frame handler)))
  | Jtrywith (body, id, handler) ->
      if Jclosure.occurs_var id handler then
        let idx_exn, stack_frame_with_id = add_var Boxed_value id stack_frame in
        Mtrywith (transl add_function result_kind stack_frame body,
                  Some idx_exn,
                  transl add_function result_kind stack_frame_with_id handler)
      else
        Mtrywith (transl add_function result_kind stack_frame body,
                  None,
                  transl add_function result_kind stack_frame handler)
  | Jifthenelse (Jprim (Pnot, [arg], _), ifso, ifnot) ->
      transl add_function result_kind stack_frame (Jifthenelse (arg, ifnot, ifso))
  | Jifthenelse (cond, ifso, Jstaticfail (nfail, [])) ->
      exit_if_false add_function result_kind stack_frame cond
        (transl add_function result_kind stack_frame ifso) nfail
  | Jifthenelse (cond, Jstaticfail (nfail, []), ifno) ->
      exit_if_true add_function result_kind stack_frame cond nfail
        (transl add_function result_kind stack_frame ifno)
  | Jifthenelse (Jprim (Psequand, _, _) as cond, ifso, ifnot) ->
      let raise_num = next_raise_count () in
      make_catch
        raise_num
        (exit_if_false add_function result_kind stack_frame cond
           (transl add_function result_kind stack_frame ifso) raise_num)
        (transl add_function result_kind stack_frame ifnot)
  | Jifthenelse (Jprim (Psequor, _, _) as cond, ifso, ifnot) ->
      let raise_num = next_raise_count () in
      make_catch
        raise_num
        (exit_if_true add_function result_kind stack_frame cond raise_num
           (transl add_function result_kind stack_frame ifnot))
        (transl add_function result_kind stack_frame ifso)
  | Jifthenelse (Jifthenelse (cond, condso, condnot), ifso, ifnot) ->
      let num_true = next_raise_count () in
      make_catch
        num_true
        (make_catch2
           (fun shared_false ->
             Mifthenelse (transl add_function Normalized_int stack_frame cond,
                          exit_if_true add_function result_kind stack_frame condso num_true shared_false,
                          exit_if_true add_function result_kind stack_frame condnot num_true shared_false))
           (transl add_function result_kind stack_frame ifnot))
        (transl add_function result_kind stack_frame ifso)
  | Jifthenelse (cond, ifso, ifnot) ->
      Mifthenelse (transl add_function Normalized_int stack_frame cond,
                   transl add_function result_kind stack_frame ifso,
                   transl add_function result_kind stack_frame ifnot)
  | Jsequence (j1, j2) ->
      sequence
        (remove_unit (transl add_function Boxed_value stack_frame j1))
        (transl add_function result_kind stack_frame j2)
  | Jwhile (cond, body, lii) ->
      Mwhile (transl add_function Normalized_int stack_frame cond,
              remove_unit (transl add_function Boxed_value stack_frame body),
              lii)
      |> return_unit result_kind
  | Jfor (id, low, high, dir, body, lii) ->
      let kind =
        if Jconfig.ints_are_63_bit_long then
          Tagged_int
        else
          Unboxed_int in
      let low = transl add_function kind stack_frame low in
      let idx, stack_frame = add_var kind id stack_frame in
      let high = transl add_function kind stack_frame high in
      let idx_high, stack_frame = add_var kind (Ident.create "_dummy_") stack_frame in
      let body = remove_unit (transl add_function Boxed_value stack_frame body) in
      Msequence (Mwritelocal (kind, idx, low),
                 (Msequence (Mwritelocal (kind, idx_high, high),
                             (Mfor (idx, dir, idx_high, body, lii)))))
      |> return_unit result_kind
  | Jassign (id, expr) ->
      begin match Ident.find_same id stack_frame.elements with
      | Some (kind, index) ->
          let res = transl add_function kind stack_frame expr in
          return_unit result_kind (Mwritelocal (kind, index, res))
      | None ->
          Mnop
      end
  | Jsend (kind, meth, obj, args, dbg) ->
      let idx_obj, stack_frame = add_var Boxed_value (Ident.create "_dummy_") stack_frame in
      let obj = transl add_function Boxed_value stack_frame obj in
      let instr_obj = Mwritelocal (Boxed_value, idx_obj, obj) in
      begin match kind, args with
      | Self, _ ->
          let idx_meth, stack_frame = add_var Boxed_value (Ident.create "_dummy_") stack_frame in
          let meth = transl add_function Normalized_int stack_frame meth in
          let instr_meth = Mwritelocal (Boxed_value, idx_meth, Mdynamicoffset (Mreadlocal (Boxed_value, idx_obj), meth)) in
          let args, prefix = transl_list add_function stack_frame args in
          let call = Mapply (idx_meth, Mreadlocal (Boxed_value, idx_obj) :: args, dbg) in
          sequence
            instr_obj
            (sequence
               instr_meth
               (convert Boxed_value result_kind (sequence prefix call)))
      | Cached, cache :: pos :: args ->
          let idx_meth, stack_frame = add_var Boxed_value (Ident.create "_dummy_") stack_frame in
          let meth = transl add_function Boxed_value stack_frame meth in
          let instr_meth = Mwritelocal (Boxed_value, idx_meth, meth) in
          let idx_cache, stack_frame = add_var Boxed_value (Ident.create "_dummy_") stack_frame in
          let cache = transl add_function Boxed_value stack_frame cache in
          let instr_cache = Mwritelocal (Boxed_value, idx_cache, cache) in
          let idx_pos, stack_frame = add_var Boxed_value (Ident.create "_dummy") stack_frame in
          let pos = transl add_function Boxed_value stack_frame pos in
          let instr_pos = Mwritelocal (Boxed_value, idx_pos, pos) in
          let args, prefix = transl_list add_function stack_frame args in
          let call = Msend (idx_meth, idx_cache, idx_pos, Mreadlocal (Boxed_value, idx_obj) :: args, dbg) in
          sequence
            instr_obj
            (sequence
               instr_meth
               (sequence
                  instr_cache
                  (sequence
                     instr_pos
                     (convert Boxed_value result_kind (sequence prefix call)))))
      | _ ->
          let idx_meth, stack_frame = add_var Boxed_value (Ident.create "_dummy_") stack_frame in
          let meth = transl add_function Boxed_value stack_frame meth in
          let get_public_method =
            { fl_class = "org.ocamljava.runtime.primitives.stdlib.Obj";
              fl_method = "caml_get_public_method" } in
          let instr_meth = Mwritelocal (Boxed_value,
                                        idx_meth,
                                        Mcall (get_public_method,
                                               [ Mreadlocal (Boxed_value, idx_obj) ; meth ],
                                               [ Boxed_value ; Boxed_value ],
                                               Some Boxed_value,
                                               Debuginfo.none)) in
          let args, prefix = transl_list add_function stack_frame args in
          let call = Mapply (idx_meth, Mreadlocal (Boxed_value, idx_obj) :: args, dbg) in
          sequence
            instr_obj
            (sequence
               instr_meth
               (convert Boxed_value result_kind (sequence prefix call)))
      end
  | Jnop ->
      Mboxedunit

and transl_list add_function ?(kinds = None) stack_frame l =
  let result_kinds =
    match kinds with
    | Some k -> assert (List.length k = List.length l); k
    | None -> repeat_kind Boxed_value (List.length l) in
  if list_contains_try_with l then
    let args, prefix, _ =
      List.fold_right2
        (fun kind expr (args, prefix, frame) ->
          if Jclosure.is_pure_jlambda expr then begin
            let res = transl add_function kind frame expr in
            res :: args, prefix, frame
          end else begin
            let index, frame = add_var kind (Ident.create "_dummy_") frame in
            let res = transl add_function kind frame expr in
            Mreadlocal (kind, index) :: args,
            Msequence (prefix, Mwritelocal (kind, index, res)),
            frame
          end)
        result_kinds
        l
        ([], Mnop, stack_frame) in
    args, prefix
  else
    List.map2
      (fun kind expr -> transl add_function kind stack_frame expr)
      result_kinds
      l,
    Mnop

and make_catch nfail body handler = match body with
  | Mstaticfail (nfail', []) when nfail = nfail' -> handler
  | _ -> Mstaticcatch (nfail, body, handler)

and make_catch2 mk_body handler =
  match handler with
  | Mstaticfail (_, [])
  | Mconst (Lambda_const (Const_base (Const_int _)))
  | Mconst (Lambda_const (Const_base (Const_char _)))
  | Mconst (Lambda_const (Const_block (_,  [])))
  | Mconst (Lambda_const (Const_pointer _)) ->
      mk_body handler
  | _ ->
      let nfail = next_raise_count () in
      make_catch
        nfail
        (mk_body (Mstaticfail (nfail, [])))
        handler

and exit_if_true add_function result_kind stack_frame cond nfail otherwise =
  match cond with
  | Jconst (Lambda_const (Const_pointer 0)) -> otherwise
  | Jconst (Lambda_const (Const_pointer 1)) -> Mstaticfail (nfail, [])
  | Jprim(Psequor, [arg1; arg2], _) ->
      exit_if_true add_function result_kind stack_frame arg1 nfail
        (exit_if_true add_function result_kind stack_frame arg2 nfail otherwise)
  | Jprim(Psequand, _, _) ->
      begin match otherwise with
      | Mstaticfail (raise_num, []) ->
          exit_if_false add_function result_kind stack_frame cond (Mstaticfail (nfail, [])) raise_num
      | _ ->
          let raise_num = next_raise_count () in
          make_catch
            raise_num
            (exit_if_false add_function result_kind stack_frame cond (Mstaticfail (nfail, [])) raise_num)
            otherwise
      end
  | Jprim(Pnot, [arg], _) ->
      exit_if_false add_function result_kind stack_frame arg otherwise nfail
  | Jifthenelse (cond, ifso, ifnot) ->
      make_catch2
        (fun shared ->
          Mifthenelse (transl add_function Normalized_int stack_frame cond,
                       exit_if_true add_function result_kind stack_frame ifso nfail shared,
                       exit_if_true add_function result_kind stack_frame ifnot nfail shared))
        otherwise
  | _ ->
      Mifthenelse (transl add_function Normalized_int stack_frame cond,
                   Mstaticfail (nfail, []),
                   otherwise)

and exit_if_false add_function result_kind stack_frame cond otherwise nfail =
  match cond with
  | Jconst (Lambda_const (Const_pointer 0)) -> Mstaticfail (nfail, [])
  | Jconst (Lambda_const (Const_pointer 1)) -> otherwise
  | Jprim(Psequand, [arg1; arg2], _) ->
      exit_if_false add_function result_kind stack_frame arg1
        (exit_if_false add_function result_kind stack_frame arg2 otherwise nfail) nfail
  | Jprim(Psequor, _, _) ->
      begin match otherwise with
      | Mstaticfail (raise_num,[]) ->
          exit_if_true add_function result_kind stack_frame cond raise_num (Mstaticfail (nfail, []))
      | _ ->
          let raise_num = next_raise_count () in
          make_catch
            raise_num
            (exit_if_true add_function result_kind stack_frame cond raise_num (Mstaticfail (nfail, [])))
            otherwise
      end
  | Jprim(Pnot, [arg], _) ->
      exit_if_true add_function result_kind stack_frame arg nfail otherwise
  | Jifthenelse (cond, ifso, ifnot) ->
      make_catch2
        (fun shared ->
          Mifthenelse (transl add_function Normalized_int stack_frame cond,
                       exit_if_false add_function result_kind stack_frame ifso shared nfail,
                       exit_if_false add_function result_kind stack_frame ifnot shared nfail))
        otherwise
  | _ ->
      Mifthenelse (transl add_function Normalized_int stack_frame cond,
                   otherwise,
                   Mstaticfail (nfail, []))

and transl_switch add_function result_kind stack_frame arg sw =
  if Array.length sw.js_actions = 1 then
    transl add_function result_kind stack_frame sw.js_actions.(0)
  else
    let kind = if (Array.length sw.js_index_blocks) = 0 then Normalized_int else Boxed_value in
    Mswitch (transl add_function kind stack_frame arg,
             sw.js_index_consts,
             sw.js_index_blocks,
             Array.map (transl add_function result_kind stack_frame) sw.js_actions,
             (match result_kind with
             | Boxed_value -> Mboxedunit
             | Tagged_int -> Mconst (Lambda_const (Const_base (Const_int 1)))
             | Normalized_int -> Munit
             | Unboxed_int -> Munit
             | Unboxed_int32 -> Mconst (Lambda_const (Const_base (Const_int32 0l)))
             | Unboxed_int64 -> Mconst (Lambda_const (Const_base (Const_int64 0L)))
             | Unboxed_nativeint -> Mconst (Lambda_const (Const_base (Const_nativeint 0n)))
             | Unboxed_float -> Mconst (Lambda_const (Const_base (Const_float "1.0")))
             | Unboxed_instance _ -> Mboxedunit
             | Unboxed_java_array _ -> convert Boxed_value result_kind Mboxedunit))

and transl_letrec add_function result_kind stack_frame bindings body =
  let bsz =
    List.map (fun (id, exp) -> (id, exp, expr_size Ident.empty exp)) bindings in
  let rec init_blocks stack_frame = function
    | [] -> fill_nonrec stack_frame bsz
    | (id, _exp, ((RHS_block sz | RHS_floatblock sz) as rhs)) :: rem ->
        let idx, stack_frame' = add_var Boxed_value id stack_frame in
        let prim_name =
          match rhs with
          | RHS_block _ -> "caml_alloc_dummy"
          | RHS_floatblock _ -> "caml_alloc_dummy_float"
          | RHS_nonrec -> assert false in
        let expr =
          Mprim (Pccall (default_prim prim_name),
                 [mk_boxed_int sz],
                 Debuginfo.none) in
        sequence
          (Mwritelocal (Boxed_value, idx, expr))
          (init_blocks stack_frame' rem)
    | (id, _, RHS_nonrec) :: tail ->
        let idx, stack_frame' = add_var Boxed_value id stack_frame in
        sequence
          (Mwritelocal (Boxed_value, idx, Mboxedunit))
          (init_blocks stack_frame' tail)
  and fill_nonrec stack_frame = function
    | [] -> fill_blocks stack_frame bsz
    | (_id, _exp, (RHS_block _ | RHS_floatblock _)) :: rem ->
        fill_nonrec stack_frame rem
    | (id, exp, RHS_nonrec) :: rem ->
        let expr = transl add_function Boxed_value stack_frame exp in
        let idx, stack_frame' = add_var Boxed_value id stack_frame in
        sequence
          (Mwritelocal (Boxed_value, idx, expr))
          (fill_nonrec stack_frame' rem)
  and fill_blocks stack_frame = function
    | [] -> transl add_function result_kind stack_frame body
    | (id, exp, (RHS_block _ | RHS_floatblock _)) :: rem ->
        begin match Ident.find_same id stack_frame.elements with
        | Some (kind, idx) ->
            let idx_tmp, _ =
              add_var Boxed_value (Ident.create "_dummy_") stack_frame in
            let store_tmp =
              Mwritelocal (Boxed_value, idx_tmp, transl add_function Boxed_value stack_frame exp) in
            let update = Mprim (Pccall (default_prim "caml_update_dummy"),
                                [convert kind Boxed_value (Mreadlocal (kind, idx));
                                 Mreadlocal (Boxed_value, idx_tmp)],
                                Debuginfo.none) in
            sequence
              store_tmp
              (sequence
                 update
                 (sequence
                    (Mpop Boxed_value)
                    (fill_blocks stack_frame rem)))
        | None ->
            Mnop
        end
    | (_id, _exp, RHS_nonrec) :: rem ->
        fill_blocks stack_frame rem
  in init_blocks stack_frame bsz

type add_function = string -> string -> (Ident.t * Lambda.repr) list -> Lambda.repr -> Jlambda.jlambda -> unit

let translate add_function entry params repr body =
  record_global_uses body;
  let stack_frame = make_frame entry params in
  let result_kind = kind_of_repr repr in
  transl add_function result_kind stack_frame body

let report_error ppf = function
  | Special_primitive_string name ->
      Format.fprintf ppf
        "First argument of primitive %S should be a constant string"
        name
