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

external make : t option -> string -> t =
  "ocamljava_threadgroup_make"

external active_count : t -> int32 =
  "ocamljava_threadgroup_active_count"

external active_group_count : t -> int32 =
  "ocamljava_threadgroup_active_group_count"

external destroy : t -> unit =
  "ocamljava_threadgroup_destroy"

external get_max_priority : t -> int32 =
  "ocamljava_threadgroup_get_max_priority"

external get_name : t -> string =
  "ocamljava_threadgroup_get_name"

external get_parent : t -> t option =
  "ocamljava_threadgroup_get_parent"

external interrupt : t -> unit =
  "ocamljava_threadgroup_interrupt"

external is_daemon : t -> bool =
  "ocamljava_threadgroup_is_daemon"

external is_destroyed : t -> bool =
  "ocamljava_threadgroup_is_destroyed"

external parent_of : t -> t -> bool =
  "ocamljava_threadgroup_parent_of"

external set_daemon : t -> bool -> unit =
  "ocamljava_threadgroup_set_daemon"

external set_max_priority : t -> int32 -> unit =
  "ocamljava_threadgroup_set_max_priority"
