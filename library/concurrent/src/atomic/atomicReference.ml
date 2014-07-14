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

external make : 'a -> 'a t =
  "ocamljava_atomicreference_make"

external compare_and_set : 'a t -> 'a -> 'a -> bool =
  "ocamljava_atomicreference_compare_and_set"

external get : 'a t -> 'a =
  "ocamljava_atomicreference_get"

external get_and_set : 'a t -> 'a -> 'a =
  "ocamljava_atomicreference_get_and_set"

external lazy_set : 'a t -> 'a -> unit =
  "ocamljava_atomicreference_lazy_set"

external set : 'a t -> 'a -> unit =
  "ocamljava_atomicreference_set"

external weak_compare_and_set : 'a t -> 'a -> 'a -> bool =
  "ocamljava_atomicreference_weak_compare_and_set"
