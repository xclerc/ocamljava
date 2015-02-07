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

open Format
open Jlambda


let rec java_type ppf (jt : java_type) =
  match jt with
  | `Boolean  -> fprintf ppf "boolean"
  | `Byte     -> fprintf ppf "byte"
  | `Char     -> fprintf ppf "char"
  | `Double   -> fprintf ppf "double"
  | `Float    -> fprintf ppf "float"
  | `Int      -> fprintf ppf "int"
  | `Long     -> fprintf ppf "long"
  | `Short    -> fprintf ppf "short"
  | `Void     -> fprintf ppf "void"
  | `Class cn -> fprintf ppf "%s" cn
  | `Array at -> fprintf ppf "%a[]" java_type (at :> java_type)

let non_void_java_type ppf (jt : non_void_java_type) =
  java_type ppf (jt :> java_type)

let array_type ppf (jt : array_type) =
  java_type ppf (jt :> java_type)

let non_void_java_type_list ppf l =
  List.iter
    (fun jt ->
      fprintf ppf "@ %a" non_void_java_type jt)
    l

let ellipsis ppf = function
  | true -> fprintf ppf "@ varargs"
  | false -> ()

let proxy_kind ppf = function
  | Custom_class_loader  -> fprintf ppf "loader"
  | System_class_loader  -> fprintf ppf "system"
  | Runtime_class_loader -> fprintf ppf "runtime"

let rec java_primitive ppf = function
  | Java_constructor (cn, params, ellip) ->
      fprintf ppf "constructor@ %s%a%a"
        cn
        non_void_java_type_list params
        ellipsis ellip
  | Java_array (jt, { jpad_total; jpad_init}) ->
      fprintf ppf "array@ %a@ %d@ %d"
        array_type jt
        jpad_total
        jpad_init
  | Java_array_get jt ->
      fprintf ppf "array_get@ %a"
        array_type jt
  | Java_array_set jt ->
      fprintf ppf "array_set@ %a"
        array_type jt
  | Java_array_length jt ->
      fprintf ppf "array_length@ %a"
        array_type jt
  | Java_array_to_object jt ->
      fprintf ppf "array_to_object@ %a"
        array_type jt
  | Java_array_of_object jt ->
      fprintf ppf "array_of_object@ %a"
        array_type jt
  | Java_method (cn, mn, mc, ck, params, ellip, return) ->
      let return =
        match mc with
        | Jtypes.Bare_call     -> return
        | Jtypes.Pop_result    -> `Void
        | Jtypes.Push_instance -> `Class cn in
      fprintf ppf "method@ %a%a@ %s.%s%a%a@ %a"
        call_kind ck
        method_call mc
        cn
        mn
        non_void_java_type_list params
        ellipsis ellip
        java_type return
  | Java_get (cn, fn, fk, jt) ->
      fprintf ppf "get@ %a@ %s.%s@ %a"
        field_kind fk
        cn
        fn
        non_void_java_type jt
  | Java_set (cn, fn, fk, jt) ->
      fprintf ppf "set@ %a@ %s.%s@ %a"
        field_kind fk
        cn
        fn
        non_void_java_type jt
  | Java_get_null ->
      fprintf ppf "get_null"
  | Java_is_null ->
      fprintf ppf "is_null"
  | Java_is_not_null ->
      fprintf ppf "is_not_null"
  | Java_equal ->
      fprintf ppf "equal"
  | Java_not_equal ->
      fprintf ppf "not_equal"
  | Java_instanceof jt ->
      fprintf ppf "instanceof@ %a"
        non_void_java_type jt
  | Java_cast jt ->
      fprintf ppf "cast@ %a"
        non_void_java_type jt
  | Java_class jt ->
      fprintf ppf "class@ %a"
        java_type jt
  | Java_throw ->
      fprintf ppf "throw"
  | Java_synchronized (kind, idx) ->
      fprintf ppf "synchronized@ %a@ %d"
        sync_kind kind
        idx
  | Java_proxy { jpp_kind; jpp_interface; jpp_interfaces; jpp_mapping } ->
      let string_list ppf l =
        List.iter
          (fun s ->
            fprintf ppf "@ %s" s)
          l in
      let bindings ppf l =
        List.iter
          (fun { jppb_interface; jppb_method; jppb_parameters; jppb_ocaml_name } ->
            fprintf ppf "%s.%s%a@ ->@ %s"
              jppb_interface
              jppb_method
              non_void_java_type_list jppb_parameters
              jppb_ocaml_name)
          l in
      let mapping ppf l =
        fprintf ppf "@ {%a }" bindings l in
      fprintf ppf "proxy@ %a%s%a%a"
        proxy_kind jpp_kind
        jpp_interface
        string_list jpp_interfaces
        mapping jpp_mapping
and call_kind ppf = function
  | Static_call    -> fprintf ppf "static"
  | Interface_call -> fprintf ppf "interface"
  | Virtual_call   -> fprintf ppf "virtual"
and method_call ppf = function
  | Jtypes.Bare_call     -> fprintf ppf ""
  | Jtypes.Pop_result    -> fprintf ppf "[exec]"
  | Jtypes.Push_instance -> fprintf ppf "[chain]"
and field_kind ppf = function
  | Static_field   -> fprintf ppf "static"
  | Instance_field -> fprintf ppf "instance"
and sync_kind ppf = function
  | Inlined_sync  -> fprintf ppf "inlined"
  | Function_sync -> fprintf ppf "function"

let loop_inlining_info ppf = function
  | Some x -> fprintf ppf "[inlining %d]" x
  | None -> fprintf ppf "[no inlining]"

let meth_kind ppf = function
  | Lambda.Self -> fprintf ppf "self"
  | Lambda.Public -> fprintf ppf "public"
  | Lambda.Cached -> fprintf ppf "cached"

let const ppf = function
  | Lambda_const lsc -> Printlambda.structured_constant ppf lsc
  | Const_targetint i -> Targetint.print ppf i
  | Const_null _ -> fprintf ppf "@[<2>(null)@]"
  | Const_javastring s -> fprintf ppf "@[<2>(javastring@ %S)@]" s

let rec jlambda ppf = function
  | Jvar id ->
      Ident.print ppf id
  | Jconst cst ->
      const ppf cst
  | Jdirect_apply ({ fl_class; fl_method }, args, params, return, _) ->
      let repr_list ppf l =
        List.iter
          (fun r ->
            fprintf ppf "@ %a" Printlambda.repr r)
          l in
      fprintf ppf "@[<2>(directapply@ %s@ %s%a@ %a%a)@]"
        fl_class
        fl_method
        repr_list params
        Printlambda.repr return
        jlambda_list args
  | Jgeneric_apply (func, args, _) ->
      fprintf ppf "@[<2>(genericapply@ %a%a)@]"
        jlambda func
        jlambda_list args
  | Jclosure (funcs, vars) ->
      let ident_repr_pairs ppf = function
        | (id, r) :: tl ->
            fprintf ppf "%a/%a" Ident.print id Printlambda.repr r;
            List.iter
              (fun (id, r) ->
                fprintf ppf "@ %a/%a" Ident.print id Printlambda.repr r)
              tl
        | [] ->
            () in
      let fun_decls ppf l =
        List.iter
          (fun { label = { fl_class; fl_method }; arity; params; return; body; dbg } ->
            ignore dbg;
            fprintf ppf "@ %s@ %s@ %d@ (%a):%a@ %a"
              fl_class
              fl_method
              arity
              ident_repr_pairs params
              Printlambda.repr return
              jlambda body)
          l in
      fprintf ppf "@[<2>(closure%a%a)@]"
        fun_decls funcs
        jlambda_list vars
  | Joffset (j, n) ->
      fprintf ppf "@[<2>(offset@ %d@ %a)@]"
        n
        jlambda j
  | Jlet (id, expr, body) ->
      let rec letbody = function
        | Jlet (id, expr, body) ->
            fprintf ppf "@ @[<2>%a@ %a@]"
              Ident.print id
              jlambda expr;
            letbody body
        | expr -> expr in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]"
        Ident.print id
        jlambda expr;
      fprintf ppf ")@]@ %a)@]"
        jlambda (letbody body)
  | Jletrec (id_expr_list, body) ->
      let bindings ppf = function
        | (id, j) :: tl ->
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id jlambda j;
            List.iter
              (fun (id, j) ->
                fprintf ppf "@[<2>%a@ %a@]" Ident.print id jlambda j)
              tl
        | [] -> () in
      fprintf ppf "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]"
        bindings id_expr_list
        jlambda body
  | Jprim (prim, args, _) ->
      fprintf ppf "@[<2>(prim@ %a%a)@]"
        Printlambda.primitive prim
        jlambda_list args
  | Jjavaprim (jprim, args, _) ->
      fprintf ppf "@[<2>(javaprim@ %a%a)@]"
        java_primitive jprim
        jlambda_list args
  | Jswitch (arg, sw) ->
      let switch ppf sw =
        let all_actions =
          Array.append
            (Array.mapi
               (fun i x -> i, true, sw.js_actions.(x))
               sw.js_index_consts)
            (Array.mapi
               (fun i x -> i, false, sw.js_actions.(x))
               sw.js_index_blocks) in
        Array.iteri
          (fun i (n, const, j) ->
            if i > 0 then pp_print_space ppf ();
            fprintf ppf "@[<hv 1>case %s %i:@ %a@]"
              (if const then "int" else "tag")
              n
              jlambda j)
          all_actions in
      fprintf ppf "@[<1>(switch %a@ @[<v 0>%a@])@]"
        jlambda arg
        switch sw
  | Jstaticfail (nfail, args) ->
      fprintf ppf "@[<2>(staticfail@ %d%a)@]"
        nfail
        jlambda_list args
  | Jstaticcatch (nfail, ids, body, handler) ->
      let idents ppf l =
        List.iter
          (fun id ->
            fprintf ppf "@ %a" Ident.print id)
          l in
      fprintf ppf "@[(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        jlambda body
        nfail
        idents ids
        jlambda handler
  | Jtrywith (body, id, handler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with@ %a@ %a)@]"
        jlambda body
        Ident.print id
        jlambda handler
  | Jifthenelse (cond, ifso, ifno) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]"
        jlambda cond
        jlambda ifso
        jlambda ifno
  | Jsequence (j1, j2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]"
        jlambda j1
        sequence j2
  | Jwhile (cond, body, inlining_info) ->
      fprintf ppf "@[<2>(while@ %a@ %a@ %a)@]"
        loop_inlining_info inlining_info
        jlambda cond
        jlambda body
  | Jfor (id, lo, hi, dir, body, inlining_info) ->
      fprintf ppf "@[<2>(for@ %a@ %a@ %a@ %s@ %a@ %a)@]"
        loop_inlining_info inlining_info
        Ident.print id
        jlambda lo
        (if dir = Asttypes.Upto then "to" else "downto")
        jlambda hi
        jlambda body
  | Jassign (id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]"
        Ident.print id
        jlambda expr
  | Jsend (kind, meth, obj, args, _) ->
      fprintf ppf "@[<2>(send@ %a@ %a@ %a%a)@]"
        meth_kind kind
        jlambda obj
        jlambda meth
        jlambda_list args
  | Jnop ->
      fprintf ppf "@[<2>(nop)@]"
and jlambda_list ppf l =
  List.iter
    (fun j ->
      fprintf ppf "@ %a" jlambda j)
    l
and sequence ppf = function
  | Jsequence (j1, j2) ->
      fprintf ppf "%a@ %a"
        sequence j1
        sequence j2
  | j ->
      jlambda ppf j
