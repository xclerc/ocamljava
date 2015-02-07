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
open Macroinstr
open Jlambda


let value_kind ppf = function
  | Boxed_value           -> fprintf ppf "value"
  | Tagged_int            -> fprintf ppf "tagged_int"
  | Normalized_int        -> fprintf ppf "normalized_int"
  | Unboxed_int           -> fprintf ppf "int"
  | Unboxed_int32         -> fprintf ppf "int32"
  | Unboxed_int64         -> fprintf ppf "int64"
  | Unboxed_nativeint     -> fprintf ppf "nativeint"
  | Unboxed_float         -> fprintf ppf "float"
  | Unboxed_instance cn   -> fprintf ppf "instance:%s" cn
  | Unboxed_java_array at ->
      (at :> Jlambda.java_type)
      |> Jlambda.unconvert_java_type
      |> BaristaLibrary.Descriptor.external_utf8_of_java_type
      |> BaristaLibrary.UTF8.to_string
      |> fprintf ppf "array:%s"

let value_kinds ppf l =
  List.iter
    (fun vk ->
      fprintf ppf "@ %a" value_kind vk)
    l

let rec expr ppf = function
  | Mconst cst ->
      Printjlambda.const ppf cst
  | Moffset (e, n) ->
      fprintf ppf "@[<2>(offset@ %a@ %d)@]"
        expr e
        n
  | Mdynamicoffset (e1, e2) ->
      fprintf ppf "@[<2>(dynamicoffset@ %a@ %a)@]"
        expr e1
        expr e2
  | Mcreateglobal (s, cn) ->
      fprintf ppf "@[<2>(createglobal@ %s@ %s)@]"
        s
        cn
  | Mclosure (n, funcs, vars) ->
      let fun_decls ppf l =
        List.iter
          (fun { mfun_label = { fl_class; fl_method };
                 mfun_ocaml_arity; mfun_java_arity } ->
            fprintf ppf "@ (%s@ %s@ %d@ %d)"
              fl_class
              fl_method
              mfun_ocaml_arity
              mfun_java_arity)
          l in
      fprintf ppf "@[<2>(closure@ %d%a%a)@]"
        n
        fun_decls funcs
        exprs vars
  | Mreadlocal (kind, n) ->
      fprintf ppf "@[<2>(readlocal@ %a@ %d)@]"
        value_kind kind
        n
  | Mwritelocal (kind, n, e) ->
      fprintf ppf "@[<2>(writelocal@ %a@ %d@ %a)@]"
        value_kind kind
        n
        expr e
  | Mpoptolocal (kind, n) ->
      fprintf ppf "@[<2>(poptolocal@ %a@ %d)@]"
        value_kind kind
        n
  | Mcall ({ fl_class; fl_method }, args, kinds, Some return_kind, _) ->
      fprintf ppf "@[<2>(call@ %s@ %s%a%a@ %a)@]"
        fl_class
        fl_method
        exprs args
        value_kinds kinds
        value_kind return_kind
  | Mcall ({ fl_class; fl_method }, args, kinds, None, _) ->
      fprintf ppf "@[<2>(call@ %s@ %s%a%a@ none)@]"
        fl_class
        fl_method
        exprs args
        value_kinds kinds
  | Mprim (prim, args, _) ->
      fprintf ppf "@[<2>(prim@ %a%a)@]"
        Printlambda.primitive prim
        exprs args
  | Mjavaprim (jprim, args, _) ->
      fprintf ppf "@[<2>(prim@ %a%a)@]"
        Printjlambda.java_primitive jprim
        exprs args
  | Mapply (clos, args, _) ->
      fprintf ppf "@[<2>(apply@ %d%a)@]"
        clos
        exprs args
  | Msend (tag, cache, pos, args, _) ->
      fprintf ppf "@[<2>(send@ %d@ %d@ %d%a)@]"
        tag
        cache
        pos
        exprs args
  | Msequence (e1, e2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]"
        expr e1
        sequence e2
  | Mifthenelse (cond, ifso, ifno) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]"
        expr cond
        expr ifso
        expr ifno
  | Mwhile (cond, body, inlining_info) ->
      fprintf ppf "@[<2>(while@ %a@ %a@ %a)@]"
        Printjlambda.loop_inlining_info inlining_info
        expr cond
        expr body
  | Mfor (n, dir, n', body, inlining_info) ->
      fprintf ppf "@[<2>(for@ %a@ %d@ %s@ %d@ %a)@]"
        Printjlambda.loop_inlining_info inlining_info
        n
        (if dir = Asttypes.Upto then "to" else "downto")
        n'
        expr body
  | Munit ->
      fprintf ppf "@[<2>(unit)@]"
  | Mboxedunit ->
      fprintf ppf "@[<2>(boxedunit)@]"
  | Mpop kind ->
      fprintf ppf "@[<2>(pop@ %a)@]"
        value_kind kind
  | Mnop ->
      fprintf ppf "@[<2>(nop)@]"
  | Mtrywith (body, Some n, handler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %d@ %a)@]"
        expr body
        n
        expr handler
  | Mtrywith (body, None, handler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with@ %a)@]"
        expr body
        expr handler
  | Mstaticfail (nfail, args) ->
      fprintf ppf "@[<2>(staticfail@ %d%a)@]"
        nfail
        exprs args
  | Mstaticcatch (nfail, body, handler) ->
      fprintf ppf "@[(catch@ %a@;<1 -1>with %d@ %a)@]"
        expr body
        nfail
        expr handler
  | Mswitch (arg, index_const, index_block, acts, default) ->
      let couple const ppf (n, body) =
        fprintf ppf "@[<hv 1>case %s %i:@ %a@]"
          (if const then "const" else "block")
          n
          expr body in
      let couples const ppf a =
        let len = Array.length a in
        for i = 0 to pred len do
          if i > 0 then fprintf ppf "@ ";
          couple const ppf a.(i)
        done in
      fprintf ppf "@[<1>(switch %a@ @[<v 0>%a%a@]@ default:@ %a)@]"
        expr arg
        (couples true) (Array.mapi (fun i x -> i, acts.(x)) index_const)
        (couples false) (Array.mapi (fun i x -> i, acts.(x)) index_block)
        expr default
  | Mconvert (src, dst, e) ->
      fprintf ppf "@[<2>(convert@ %a@ %a@ %a)@]"
        value_kind src
        value_kind dst
        expr e
  | Minit idx ->
      (match idx with
      | Some x ->
          fprintf ppf "@[<2>(init-begin@ %d)@]" x
      | None ->
          fprintf ppf "@[<2>(init-end)@]")
and exprs ppf l =
  List.iter
    (fun e ->
      fprintf ppf "@ %a" expr e)
    l
and sequence ppf = function
  | Msequence (e1, e2) ->
      fprintf ppf "%a@ %a"
        sequence e1
        sequence e2
  | e ->
      expr ppf e

let macroinstr ppf fundecls =
  let idents ppf l =
    List.iter
      (fun id ->
        fprintf ppf "@ %a" Ident.print id)
      l in
  let reprs ppf l =
    List.iter
      (fun r ->
        fprintf ppf "@ %a" Printlambda.repr r)
      l in
  let fundecl ppf { fun_name; fun_args; fun_params; fun_return; fun_body } =
    fprintf ppf "@[(function@ %s%a%a@ %a@ %a)@]"
      fun_name
      idents fun_args
      reprs fun_params
      Printlambda.repr fun_return
      expr fun_body in
  match fundecls with
  | hd :: tl ->
      fundecl ppf hd;
      List.iter
        (fun fd ->
          fprintf ppf "@ %a" fundecl fd)
        tl
  | [] ->
      ()
