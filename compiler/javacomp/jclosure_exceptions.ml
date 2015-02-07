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

open Jlambda

type exception_tag =
  | Predefined_exception of Ident.t
  | Defined_exception of Ident.t * int
  | Any_exception

let same_exception_tag x y =
  match x, y with
  | Predefined_exception id, Predefined_exception id' ->
      Ident.same id id'
  | Defined_exception (id, idx), Defined_exception (id', idx') ->
      (Ident.same id id') && (idx = idx')
  | Any_exception, Any_exception ->
      true
  | _ ->
      false

let rec extract_try_with_handlers acc = function
  | Jifthenelse (Jprim (Lambda.Pintcomp Lambda.Ceq,
                        [_; Jprim (Lambda.Pgetglobal id, [], _)],
                        _),
                 ifso,
                 ifno) ->
      let elem = Predefined_exception id, ifso in
      extract_try_with_handlers (elem :: acc) ifno
  | Jifthenelse (Jprim (Lambda.Pintcomp Lambda.Ceq,
                        [_; Jprim (Lambda.Pfield idx,
                                   [Jprim (Lambda.Pgetglobal id, [], _)], _)],
                        _),
                 ifso,
                 ifno) ->
      let elem = Defined_exception (id, idx), ifso in
      extract_try_with_handlers (elem :: acc) ifno
  | expr ->
      let elem = Any_exception, expr in
      List.rev (elem :: acc)

let extract_try_with_handlers j =
  extract_try_with_handlers [] j

let contains_exception_tag x l =
  List.exists (same_exception_tag x) l

(* note: we are in a try/with, and thus there is nothing on the stack *)
let rec extract_raised acc = function
  | Jvar _ -> acc
  | Jconst _ -> acc
  | Jclosure _ ->
      (* do not visit those nodes that will result in different functions *)
      acc
  | Joffset _ -> acc
  | Jlet (_, j1, j2) ->
      let acc = extract_raised acc j1 in
      extract_raised acc j2
  | Jletrec (l, j) ->
      let acc = extract_raised_list acc (List.map snd l) in
      extract_raised acc j
  | Jprim (Lambda.Praise,
           [Jprim (Lambda.Pmakeblock _,
                   Jprim (Lambda.Pfield idx,
                          [Jprim (Lambda.Pgetglobal id, [], _)], _) :: _,
                   _)],
           _) ->
    let tag = Defined_exception (id, idx) in
    if contains_exception_tag tag acc then
      acc
    else
      tag :: acc
  | Jprim (Lambda.Praise,
           [Jprim (Lambda.Pmakeblock _,
                   Jprim (Lambda.Pgetglobal id, [], _) :: _,
                   _)],
           _) ->
    let tag = Predefined_exception id in
    if contains_exception_tag tag acc then
      acc
    else
      tag :: acc
  | Jswitch (j, sw) ->
      let acc = extract_raised acc j in
      extract_raised_array acc sw.js_actions
  | Jstaticfail (_, l) ->
      extract_raised_list acc l
  | Jstaticcatch (_, _, j1, j2) ->
      let acc = extract_raised acc j1 in
      extract_raised acc j2
  | Jtrywith _ ->
      acc
  | Jifthenelse (j1, j2, j3) ->
      let acc = extract_raised acc j1 in
      let acc = extract_raised acc j2 in
      extract_raised acc j3
  | Jsequence (j1, j2) ->
      let acc = extract_raised acc j1 in
      extract_raised acc j2
  | Jwhile (j1, j2, _) ->
      let acc = extract_raised acc j1 in
      extract_raised acc j2
  | Jfor (_, j1, j2, _, j3, _) ->
      let acc = extract_raised acc j1 in
      let acc = extract_raised acc j2 in
      extract_raised acc j3
  | Jassign (_, j) ->
      extract_raised acc j
  | Jnop -> acc
  | (Jdirect_apply _ | Jgeneric_apply _ | Jprim _ | Jjavaprim _ | Jsend _) ->
      (* we do not treat nested raises as the stack may not be empty anymore *)
      acc
and extract_raised_list acc l =
  List.fold_left
    (fun acc elem -> extract_raised acc elem)
    acc
    l
and extract_raised_array acc l =
  Array.fold_left
    (fun acc elem -> extract_raised acc elem)
    acc
    l

let extract_raised l = extract_raised [] l

let replace_raise_prim handlers raise_tag p l =
  try
    let _tag, _handler, use_id, nfail =
      List.find
        (fun (tag, _, _, _) ->
          (tag = Any_exception) || (same_exception_tag tag raise_tag))
        handlers in
    let params = if use_id then [p] else [] in
    Jstaticfail (nfail, params)
  with Not_found ->
    l

let rec replace_raise handlers l =
  match l with
  | Jvar _ -> l
  | Jconst _ -> l
  | Jclosure _ -> l
  | Joffset _ -> l
  | Jlet (id, j1, j2) ->
      let j1 = replace_raise handlers j1 in
      let j2 = replace_raise handlers j2 in
      Jlet (id, j1, j2)
  | Jletrec (l, j) ->
      let l =
        List.map
          (fun (id, j) -> id, replace_raise handlers j)
          l in
      let j = replace_raise handlers j in
      Jletrec (l, j)
  | Jprim (Lambda.Praise,
           [(Jprim (Lambda.Pmakeblock _,
                    Jprim (Lambda.Pfield idx,
                           [Jprim (Lambda.Pgetglobal id, [], _)], _) :: _,
                    _)) as p],
           _) ->
    replace_raise_prim handlers (Defined_exception (id, idx)) p l
  | Jprim (Lambda.Praise,
           [(Jprim (Lambda.Pmakeblock _,
                    Jprim (Lambda.Pgetglobal id, [], _) :: _,
                    _)) as p],
           _) ->
    replace_raise_prim handlers (Predefined_exception id) p l
  | Jswitch (j, sw) ->
      let j = replace_raise handlers j in
      let sw = { sw with js_actions = replace_raise_array handlers sw.js_actions } in
      Jswitch (j, sw)
  | Jstaticfail (n, l) ->
      let l = replace_raise_list handlers l in
      Jstaticfail (n, l)
  | Jstaticcatch (n, l, j1, j2) ->
      let j1 = replace_raise handlers j1 in
      let j2 = replace_raise handlers j2 in
      Jstaticcatch (n, l, j1, j2)
  | Jtrywith _ ->
      l
  | Jifthenelse (j1, j2, j3) ->
      let j1 = replace_raise handlers j1 in
      let j2 = replace_raise handlers j2 in
      let j3 = replace_raise handlers j3 in
      Jifthenelse (j1, j2, j3)
  | Jsequence (j1, j2) ->
      let j1 = replace_raise handlers j1 in
      let j2 = replace_raise handlers j2 in
      Jsequence (j1, j2)
  | Jwhile (j1, j2, lii) ->
      let j1 = replace_raise handlers j1 in
      let j2 = replace_raise handlers j2 in
      Jwhile (j1, j2, lii)
  | Jfor (id, j1, j2, dir, j3, lii) ->
      let j1 = replace_raise handlers j1 in
      let j2 = replace_raise handlers j2 in
      let j3 = replace_raise handlers j3 in
      Jfor (id, j1, j2, dir, j3, lii)
  | Jassign (id, j) ->
      let j = replace_raise handlers j in
      Jassign (id, j)
  | Jnop -> l
  | (Jdirect_apply _ | Jgeneric_apply _ | Jprim _ | Jjavaprim _ | Jsend _) ->
      (* see extract_raised *)
      l
and replace_raise_list handlers l =
  List.map (replace_raise handlers) l
and replace_raise_array handlers l =
  Array.map (replace_raise handlers) l
