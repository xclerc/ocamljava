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
    {java java.lang.ThreadGroup#Thread(java.lang.ThreadGroup, java.lang.String)}. *)

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
    {java java.lang.ThreadGroup#parentOf(java.lang.Thread)}. *)

val set_daemon : t -> bool -> unit
(** Sets the daemon status of the group. Daemon groups are automatically
    destroyed when they have neither child group, nor running thread; see
    {java java.lang.ThreadGroup#setDaemon(boolean)}. *)

val set_max_priority : t -> java_int -> unit
(** Sets the maximum priority of the group; see
    {java java.lang.ThreadGroup#setMaxPriority(int)}. *)
