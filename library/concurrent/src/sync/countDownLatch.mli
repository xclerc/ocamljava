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

(** Countdown synchronization. *)


type t = java'util'concurrent'CountDownLatch java_instance
(** The type of countdown latches, that are barrier-like synchronization
    entities. *)

val make : java_int -> t
(** [make n] returns a countdown latch waiting for [n] threads; see
    {java java.util.concurrent.CountDownLatch#CountDownLatch(int)}.

    @raise Java_exception if [n] is negative *)

val await : t -> unit
(** Waits until the coutdown reaches zero, without countingdown; see
    {java java.util.concurrent.CountDownLatch#await()}.

    @raise Java_exception if the thread is interrupted *)

val await_time : t -> java_long -> TimeUnit.t -> bool
(** [await_time c t u] is similar to [await c], except that the current
    thread will at most wait for [t] (time value whose unit is [u]); see
    {java java.util.concurrent.CountDownLatch#await(long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted *)

val count_down : t -> unit
(** Decrements the count, and then waits for the countdown to reach
    zero; see {java java.util.concurrent.CountDownLatch#countDown()}. *)

val get_count : t -> java_long
(** Returns the current count; see
    {java java.util.concurrent.CountDownLatch#getCount()}. *)


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
