(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type 'a t = java'util'concurrent'atomic'AtomicReferenceArray java_instance

type index = int32

external make : int32 -> 'a -> 'a t =
  "ocamljava_atomicreferencearray_make"

external compare_and_set : 'a t -> index -> 'a -> 'a -> bool =
  "ocamljava_atomicreferencearray_compare_and_set"

external get : 'a t -> index -> 'a =
  "ocamljava_atomicreferencearray_get"

external get_and_set : 'a t -> index -> 'a -> 'a =
  "ocamljava_atomicreferencearray_get_and_set"

external lazy_set : 'a t -> index -> 'a -> unit =
  "ocamljava_atomicreferencearray_lazy_set"

external length : 'a t -> int32 =
  "ocamljava_atomicreferencearray_length"

external set : 'a t -> index -> 'a -> unit =
  "ocamljava_atomicreferencearray_set"

external weak_compare_and_set : 'a t -> index -> 'a -> 'a -> bool =
  "ocamljava_atomicreferencearray_weak_compare_and_set"


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
