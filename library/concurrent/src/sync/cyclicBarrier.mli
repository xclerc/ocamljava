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

(** Reusable barriers. *)


type t = java'util'concurrent'CyclicBarrier java_instance
(** The type of cyclic barriers, that are barriers possibly re-used once
    all waiting threads have restarted. *)

val make : java_int -> t
(** [make n] returns a barrier waiting for [n] threads; see
    {java java.util.concurrent.CyclicBarrier#CyclicBarrier(int)}.

    @raise Java_exception if [n] is negative *)

val await : t -> java_int
(** Waits until all threads have reached the barrier; see
    {java java.util.concurrent.CyclicBarrier#await()}.

    @raise Java_exception if the barrier is broken
    @raise Java_exception if the thread is interrupted *)

val await_time : t -> java_long -> TimeUnit.t -> java_int
(** [await_time b t u] is similar to [await b], except that the current
    thread will at most wait for [t] (time value whose unit is [u]); see
    {java java.util.concurrent.CyclicBarrier#await(long,java.util.concurrent.TimeUnit)}.

    @Raises Java_exception if the barrier is broken.
    @Raises Java_exception if the thread is interrupted.
    @Raises Java_exception if time has elapsed without gathering all
                           threads *)

val get_number_waiting : t -> java_int
(** Returns the number of threads currently waiting on the barrier; see
    {java java.util.concurrent.CyclicBarrier#getNumberWaiting()}. *)

val get_parties : t -> java_int
(** Returns the number of threads to be waited on the barrier; see
    {java java.util.concurrent.CyclicBarrier#getParties()}. *)

val is_broken : t -> bool
(** Tests whether the barrier is broken. A barrier is broken if {!reset}
    when threads are waiting on it; see
    {java java.util.concurrent.CyclicBarrier#isBroken()}. *)

val reset : t -> unit
(** Resets the barrier to its original state. The barrier will be broken
    if there are threads currently waiting on it; see
    {java java.util.concurrent.CyclicBarrier#reset()}. *)
