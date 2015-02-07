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

open Lambda
open Jlambda
open Bytecodeutils
open BaristaLibrary


module IntSet = Set.Make (struct
  type t = int
  let compare (x : t) (y : t) = Pervasives.compare x y
end)

type global_use = {
    mutable read_indices : IntSet.t;
    mutable written_indices : IntSet.t;
  }

let global_uses : (string, global_use) Hashtbl.t = Hashtbl.create 17

let reset_global_uses () =
  Hashtbl.clear global_uses

let builtin_exceptions =
  Runtimedef.builtin_exceptions
  |> Array.to_list
  |> List.map (fun str -> "caml_exn_" ^ str)

let is_predefined_exception id =
  let id = Ident.name id in
  List.exists
    (fun str -> String.compare str id = 0)
    builtin_exceptions

type use = Read | Written

let incr_global_use id use idx =
  if not (is_predefined_exception id) then begin
    let class_name = Jcompilenv.class_for_global id in
    let uses =
      try
        Hashtbl.find global_uses class_name
      with Not_found ->
        let u = { read_indices = IntSet.empty;
                  written_indices = IntSet.empty } in
        Hashtbl.add global_uses class_name u;
        u in
    match use with
    | Read    -> uses.read_indices    <- IntSet.add idx uses.read_indices
    | Written -> uses.written_indices <- IntSet.add idx uses.written_indices
  end

let rec record_global_uses = function
  | Jvar _ ->
      ()
  | Jconst _ ->
      ()
  | Jdirect_apply (_, l, _, _, _) ->
      list_record_global_uses l
  | Jgeneric_apply (j, l, _) ->
      record_global_uses j;
      list_record_global_uses l
  | Jclosure (l1, l2) ->
      list_record_global_uses (List.map (fun { body; _ } -> body) l1);
      list_record_global_uses l2
  | Joffset (j, _) ->
      record_global_uses j
  | Jlet (_, j1, j2) ->
      record_global_uses j1;
      record_global_uses j2
  | Jletrec (l, j) ->
      list_record_global_uses (List.map snd l);
      record_global_uses j
  | Jprim (Pfield idx, [Jprim (Pgetglobal id, _, _)], _) ->
      incr_global_use id Read idx
  | Jprim (Psetfield (idx, _), (Jprim (Pgetglobal id, _, _) :: l), _) ->
      incr_global_use id Written idx;
      list_record_global_uses l
  | Jprim (Pgetglobal id, l, _) ->
      incr_global_use id Read ~-1;
      list_record_global_uses l
  | Jprim (Psetglobal id, l, _) ->
      incr_global_use id Written ~-1;
      list_record_global_uses l
  | Jprim (_, l, _) ->
      list_record_global_uses l
  | Jjavaprim (_, l, _) ->
      list_record_global_uses l
  | Jswitch (j, sw) ->
      record_global_uses j;
      array_record_global_uses sw.js_actions
  | Jstaticfail (_, l) ->
      list_record_global_uses l
  | Jstaticcatch (_, _, j1, j2) ->
      record_global_uses j1;
      record_global_uses j2
  | Jtrywith (j1, _, j2) ->
      record_global_uses j1;
      record_global_uses j2
  | Jifthenelse (j1, j2, j3) ->
      record_global_uses j1;
      record_global_uses j2;
      record_global_uses j3
  | Jsequence (j1, j2) ->
      record_global_uses j1;
      record_global_uses j2
  | Jwhile (j1, j2, _) ->
      record_global_uses j1;
      record_global_uses j2
  | Jfor (_, j1, j2, _, j3, _) ->
      record_global_uses j1;
      record_global_uses j2;
      record_global_uses j3
  | Jassign (_, j) ->
      record_global_uses j
  | Jsend (_, j1, j2, l, _) ->
      record_global_uses j1;
      record_global_uses j2;
      list_record_global_uses l
  | Jnop ->
      ()
and list_record_global_uses l =
  List.iter record_global_uses l
and array_record_global_uses a =
  Array.iter record_global_uses a

let compile_global_uses () =
  let simplify_set set =
    if IntSet.mem ~-1 set then
      IntSet.singleton ~-1
    else
      set in
  let annotation_of_set set =
    Annotation.Array_value
      (set
       |> simplify_set
       |> IntSet.elements
       |> List.map (fun x -> Annotation.Int_value (Int32.of_int x))) in
  Hashtbl.fold
    (fun class_name indices acc ->
      let annotation =
        class_GlobalUses,
        [ UTF8.of_string "className",
          Annotation.String_value (UTF8.of_string class_name) ;
          UTF8.of_string "readIndices",
          annotation_of_set indices.read_indices ;
          UTF8.of_string "writtenIndices",
          annotation_of_set indices.written_indices ] in
      annotation :: acc)
    global_uses
    []
