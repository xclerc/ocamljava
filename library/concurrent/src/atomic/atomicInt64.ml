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

type t

external make : int64 -> t =
  "ocamljava_atomicint64_make"

external add_and_get : t -> int64 -> int64 =
  "ocamljava_atomicint64_add_and_get"

external compare_and_set : t -> int64 -> int64 -> bool =
  "ocamljava_atomicint64_compare_and_set"

external decrement_and_get : t -> int64 =
  "ocamljava_atomicint64_decrement_and_get"

external get : t -> int64 =
  "ocamljava_atomicint64_get"

external get_and_add : t -> int64 -> int64 =
  "ocamljava_atomicint64_get_and_add"

external get_and_decrement : t -> int64 =
  "ocamljava_atomicint64_get_and_decrement"

external get_and_increment : t -> int64 =
  "ocamljava_atomicint64_get_and_increment"

external get_and_set : t -> int64 -> int64 =
  "ocamljava_atomicint64_get_and_set"

external increment_and_get : t -> int64 =
  "ocamljava_atomicint64_increment_and_get"

external lazy_set : t -> int64 -> unit =
  "ocamljava_atomicint64_lazy_set"

external set : t -> int64 -> unit =
  "ocamljava_atomicint64_set"

external weak_compare_and_set : t -> int64 -> int64 -> bool =
  "ocamljava_atomicint64_weak_compare_and_set"
