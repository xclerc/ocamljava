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

open BaristaLibrary

type t =
  | Node of int * (t list)
  | Leaf of int * (Instruction.t list)
  | Suspended of int * (Instruction.t list lazy_t)

let size = function
  | Node (sz, _)
  | Leaf (sz, _)
  | Suspended (sz, _) -> sz

let node l =
  let sz =
    List.fold_left
      (fun acc elem ->
        acc + (size elem))
      0
      l in
  Node (sz, l)

let leaf ?(ofs = -1) l =
  let is_switch_instruction = function
    | Instruction.LOOKUPSWITCH _ | Instruction.TABLESWITCH _ -> true
    | _ -> false in
  if (ofs < 0) && (List.exists is_switch_instruction l) then
    Misc.fatal_error "Instrtree.leaf: missing offset with a list containing a switch";
  let sz = Instruction.size_of_list (Utils.max_int ofs 0) l in
  Leaf (sz, l)

let suspended sz lz =
  Suspended (sz, lz)

let flatten x =
  let res = ref [] in
  let rec visit = function
    | Node (_, l) -> List.iter visit l
    | Leaf (_, l) -> res := l :: !res
    | Suspended (_, l) -> res := (Lazy.force l) :: !res in
  visit x;
  List.flatten (List.rev !res)

let rec map f = function
  | Node (sz, l) -> Node (sz, List.map (map f) l)
  | Leaf (sz, l) -> Leaf (sz, List.map f l)
  | (Suspended _) as x -> x
