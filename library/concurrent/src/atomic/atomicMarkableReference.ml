(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

type 'a t

type mark = bool

external make : 'a -> mark -> 'a t =
  "ocamljava_atomicmarkablereference_make"

external attempt_mark : 'a t -> 'a -> mark -> bool =
  "ocamljava_atomicmarkablereference_attempt_mark"

external compare_and_set : 'a t -> 'a -> 'a -> mark -> mark -> bool =
  "ocamljava_atomicmarkablereference_compare_and_set"

external get : 'a t -> 'a * mark =
  "ocamljava_atomicmarkablereference_get"

external get_reference : 'a t -> 'a =
  "ocamljava_atomicmarkablereference_get_reference"

external is_marked : 'a t -> bool =
  "ocamljava_atomicmarkablereference_is_marked"

external set : 'a t -> 'a -> mark -> unit =
  "ocamljava_atomicmarkablereference_set"

external weak_compare_and_set : 'a t -> 'a -> 'a -> mark -> mark -> bool =
  "ocamljava_atomicmarkablereference_weak_compare_and_set"
