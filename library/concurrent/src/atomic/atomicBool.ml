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

type t

external make : bool -> t =
  "ocamljava_atomicbool_make"

external compare_and_set : t -> bool -> bool -> bool =
  "ocamljava_atomicbool_compare_and_set"

external get : t -> bool =
  "ocamljava_atomicbool_get"

external get_and_set : t -> bool -> bool =
  "ocamljava_atomicbool_get_and_set"

external lazy_set : t -> bool -> unit =
  "ocamljava_atomicbool_lazy_set"

external set : t -> bool -> unit =
  "ocamljava_atomicbool_set"

external weak_compare_and_set : t -> bool -> bool -> bool =
  "ocamljava_atomicbool_weak_compare_and_set"
