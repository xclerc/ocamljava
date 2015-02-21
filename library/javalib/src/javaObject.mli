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

(** Utility functions for Java objects. *)


(** {6 Instance creation} *)

type t = java'lang'Object java_instance
(** The type of objects. *)

val make : unit -> t
(** Returns a new {java java.lang.Object} instance. *)


(** {6 Base methods} *)

val equals : java'lang'Object java_extends -> java'lang'Object java_extends -> java_boolean
(** [equals this that] tests whether [this] and [that] are equal; see
    {java java.lang.Object#equals(java.lang.Object)}.

    @raise Java_exception if [this] is [null] *)

val get_class : java'lang'Object java_extends -> java'lang'Class java_instance
(** [get_class obj] returns the class of [obj]; see
    {java java.lang.Object#getClass()}.

    @raise Java_exception if [obj] is [null] *)

val hash_code : java'lang'Object java_extends -> java_int
(** [hash_code obj] returns the hash code of [obj]; see
    {java java.lang.Object#hashCode()}.

    @raise Java_exception if [obj] is [null] *)

val to_string : java'lang'Object java_extends -> JavaString.t
(** [to_string obj] returns the string representation of [obj]; see
    {java java.lang.Object#toString()}.

    @raise Java_exception if [obj] is [null] *)


(** {6 Synchronization} *)

val notify : java'lang'Object java_extends -> unit
(** [notify obj] wakes up a thread waiting on the monitor of [obj]; see
    {java java.lang.Object#notify()}.

    @raise Java_exception if [obj] is [null]
    @raise Java_exception if the current thread does not hold the monitor *)

val notify_all : java'lang'Object java_extends -> unit
(** [notify_all obj] wakes up all the threads waiting on the monitor of [obj]; see
    {java java.lang.Object#notifyAll()}.

    @raise Java_exception if [obj] is [null]
    @raise Java_exception if the current thread does not hold the monitor *)


val wait : java'lang'Object java_extends -> unit
(** [wait obj] waits for a notification on the monitor of [obj]; see
    {java java.lang.Object#wait()}.

    @raise Java_exception if [obj] is [null]
    @raise Java_exception if the current thread does not hold the monitor
    @raise Java_exception if the thread is interrupted during wait *)

val wait_timeout : java'lang'Object java_extends -> java_long -> unit
(** [wait_timeout obj ms] waits for a notification on the monitor of [obj],
    or a time of [ms] milliseconds has elapsed; see
    {java java.lang.Object#wait(long)}.

    @raise Java_exception if [obj] is [null]
    @raise Java_exception if the current thread does not hold the monitor
    @raise Java_exception if the thread is interrupted during wait
    @raise Java_exception if [ms] is negative *)

val wait_timeout_nanos : java'lang'Object java_extends -> java_long -> java_int -> unit
(** [wait_timeout_nanos obj ms ns] waits for a notification on the monitor of [obj],
    or a time of [to] milliseconds and [na] nanoseconds has elapsed; see
    {java java.lang.Object#wait(long, int)}.

    @raise Java_exception if [obj] is [null]
    @raise Java_exception if the current thread does not hold the monitor
    @raise Java_exception if the thread is interrupted during wait
    @raise Java_exception if [ms] is negative
    @raise Java_exception if [ns] is not in the [0]-[999999] interval *)


(** {6 Null value} *)

val null : t
(** The [null] value. *)

external is_null : 'a java_instance -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

external cast : java'lang'Object java_extends -> t =
  "%identity"
(** [cast obj] casts [obj] to a bare {java java.lang.Object} instance. *)

val wrap : 'a java_instance -> 'a java_instance option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : 'a java_instance option -> 'a java_instance
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
