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

(** Computations run in background with a delay. *)


type 'a t = java'util'concurrent'ScheduledFuture java_instance
(** The type of futures, that are computations run in background with a
    delay. *)

val cancel : 'a t -> bool -> bool
(** Same as {!Future.cancel}. *)

val get : 'a t -> 'a
(** Same as {!Future.get}. *)

val get_time : 'a t -> java_long -> TimeUnit.t -> 'a
(** Same as {!Future.get_time}. *)

val is_cancelled : 'a t -> bool
(** Same as {!Future.is_cancelled}. *)

val is_done : 'a t -> bool
(** Same as {!Future.is_done}. *)

val get_delay : 'a t -> TimeUnit.t -> java_long
(** Returns the remaining delay for the computation, in the passed time
    unit; see {java java.util.concurrent.Delayed#getDelay(java.util.concurrent.TimeUnit)}. *)


(** {6 Null value} *)

val null : 'a t
(** The [null] value. *)

external is_null : 'a t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : 'a t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : 'a t -> 'a t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : 'a t option -> 'a t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
