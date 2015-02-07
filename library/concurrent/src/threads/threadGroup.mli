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

(** Thread groups. *)


type t
(** The type of thread groups, that are sets of threads organized into a
    hierarchy (every group except the initial one has a parent).

    Threads are added to a group at creation time and cannot change group
    afterwards. *)

external make : t option -> string -> t =
  "ocamljava_threadgroup_make"
(** Returns a new group with optional parent, and name. *)

external active_count : t -> int32 =
  "ocamljava_threadgroup_active_count"
(** Returns the number of active threads in this group (including
    children groups). *)

external active_group_count : t -> int32 =
  "ocamljava_threadgroup_active_group_count"
(** Returns the number of active groups in this group (including children
    groups). *)

external destroy : t -> unit =
  "ocamljava_threadgroup_destroy"
(** Destroys the passed group and all its children groups.

    Raises [Invalid_argument] if the thread group is not empty, or has
    already been destroyed. *)

external get_max_priority : t -> int32 =
  "ocamljava_threadgroup_get_max_priority"
(** Returns the maximum priority of the group. *)

external get_name : t -> string =
  "ocamljava_threadgroup_get_name"
(** Returns the name of the group. *)

external get_parent : t -> t option =
  "ocamljava_threadgroup_get_parent"
(** Returns the parent of the group, [None] if no such group exists. *)

external interrupt : t -> unit =
  "ocamljava_threadgroup_interrupt"
(** Interrupts all thread in the group (including children groups). *)

external is_daemon : t -> bool =
  "ocamljava_threadgroup_is_daemon"
(** Tests whether the group is a daemon one. *)

external is_destroyed : t -> bool =
  "ocamljava_threadgroup_is_destroyed"
(** Tests whether the group has been destroyed. *)

external parent_of : t -> t -> bool =
  "ocamljava_threadgroup_parent_of"
(** [parent_of p c] tests whether [p] is an ancestor of [c]. *)

external set_daemon : t -> bool -> unit =
  "ocamljava_threadgroup_set_daemon"
(** Sets the daemon status of the group. Daemon groups are automatically
    destroyed when they have neither child group, nor running thread. *)

external set_max_priority : t -> int32 -> unit =
  "ocamljava_threadgroup_set_max_priority"
(** Sets the maximum priority of the group. *)
