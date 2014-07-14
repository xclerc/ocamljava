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

type stamp = int32

external make : 'a -> stamp -> 'a t =
  "ocamljava_atomicstampedreference_make"

external attempt_stamp : 'a t -> 'a -> stamp -> bool =
  "ocamljava_atomicstampedreference_attempt_stamp"

external compare_and_set : 'a t -> 'a -> 'a -> stamp -> stamp -> bool =
  "ocamljava_atomicstampedreference_compare_and_set"

external get : 'a t -> 'a * stamp =
  "ocamljava_atomicstampedreference_get"

external get_reference : 'a t -> 'a =
  "ocamljava_atomicstampedreference_get_reference"

external get_stamp : 'a t -> stamp =
  "ocamljava_atomicstampedreference_get_stamp"

external set : 'a t -> 'a -> stamp -> unit =
  "ocamljava_atomicstampedreference_set"

external weak_compare_and_set : 'a t -> 'a -> 'a -> stamp -> stamp -> bool =
  "ocamljava_atomicstampedreference_weak_compare_and_set"
