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


type t = java'lang'ThreadGroup java_instance
(** The type of thread groups, that are sets of threads organized into a
    hierarchy (every group except the initial one has a parent).

    Threads are added to a group at creation time and cannot change group
    afterwards. *)

val make : ?parent:t -> JavaString.t -> t
(** Returns a new group with optional parent, and name; see
    {java java.lang.ThreadGroup#ThreadGroup(java.lang.ThreadGroup, java.lang.String)}. *)

val active_count : t -> java_int
(** Returns the number of active threads in this group (including
    children groups); see {java java.lang.ThreadGroup#activeCount()}. *)

val active_group_count : t -> java_int
(** Returns the number of active groups in this group (including children
    groups); see {java java.lang.ThreadGroup#activeGroupCount()}. *)

val destroy : t -> unit
(** Destroys the passed group and all its children groups; see
    {java java.lang.ThreadGroup#destroy()}.

    @raise Java_exception if the thread group is not empty, or has
                          already been destroyed *)

val get_max_priority : t -> java_int
(** Returns the maximum priority of the group; see
    {java java.lang.ThreadGroup#getMaxPriority()}. *)

val get_name : t -> JavaString.t
(** Returns the name of the group; see
    {java java.lang.ThreadGroup#getName()}. *)

val get_parent : t -> t
(** Returns the parent of the group, [null] if no such group exists; see
    {java java.lang.ThreadGroup#getParent()}. *)

val interrupt : t -> unit
(** Interrupts all thread in the group (including children groups); see
    {java java.lang.ThreadGroup#interrupt()}. *)

val is_daemon : t -> bool
(** Tests whether the group is a daemon one; see
    {java java.lang.ThreadGroup#isDaemon()}. *)

val is_destroyed : t -> bool
(** Tests whether the group has been destroyed; see
    {java java.lang.ThreadGroup#isDestroyed()}. *)

val parent_of : t -> t -> bool
(** [parent_of p c] tests whether [p] is an ancestor of [c]; see
    {java java.lang.ThreadGroup#parentOf(java.lang.ThreadGroup)}. *)

val set_daemon : t -> bool -> unit
(** Sets the daemon status of the group. Daemon groups are automatically
    destroyed when they have neither child group, nor running thread; see
    {java java.lang.ThreadGroup#setDaemon(boolean)}. *)

val set_max_priority : t -> java_int -> unit
(** Sets the maximum priority of the group; see
    {java java.lang.ThreadGroup#setMaxPriority(int)}. *)

val enumerate_threads : t -> ?recurse:bool -> java'lang'Thread java_instance JavaReferenceArray.t -> int32
(** Enumerates (recursively by default) the threads in the group by
    storing them in the passed array, and returning the number of
    actually stored threads; see
    {java java.lang.ThreadGroup#enumerate(java.lang.Thread[], boolean)}. *)

val enumerate_groups : t -> ?recurse:bool -> java'lang'ThreadGroup java_instance JavaReferenceArray.t -> int32
(** Enumerates (recursively by default) the groups in the group by
    storing them in the passed array, and returning the number of
    actually stored groups; see
    {java java.lang.ThreadGroup#enumerate(java.lang.ThreadGroup[], boolean)}. *)


(** {6 Null value} *)

val null : t
(** The [null] value. *)

external is_null : t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : t -> t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : t option -> t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
