(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type location =
  | Parameter of int list
  | Field of string
  | Nowhere

module StringMap = Map.Make (String)

type t = location StringMap.t

let empty = StringMap.empty

let make l =
  List.fold_left
    (fun acc (id, _) ->
      if StringMap.mem id acc then
        acc
      else
        StringMap.add id (Field ("w" ^ id)) acc)
    StringMap.empty
    l

let size t =
  StringMap.cardinal t

let mem k t =
  StringMap.mem k t

let find k t =
  StringMap.find k t

let add k v t =
  if not (StringMap.mem k t) then
    StringMap.add k v t
  else
    t

let iter f t =
  StringMap.iter f t

let fold f t z =
  StringMap.fold f t z

let add_parameter t s l =
  t := add s (Parameter l) !t

let add_field t s f =
  t := add s (Field f) !t

let add_nowhere t s =
  t := add s Nowhere !t
