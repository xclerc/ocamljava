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

(** Lock-base condition. *)


type t = java'util'concurrent'locks'Condition java_instance
(** The type of conditions, constructed from [Lock] values.

    Operations on conditions should be called only when the lock the
    condition was constructed from is held by the current thread. *)

val await : t -> unit
(** Waits until either the condition is signal, or the current thread is
    interrupted; see
    {java java.util.concurrent.locks.Condition#await()}.

    @raise Java_exception if the current thread does not hold the
                          associated lock
    @raise Java_exception if the thread is interrupted *)

val await_time : t -> java_long -> TimeUnit.t -> bool
(** [await_time c t u] is similar to [await c], except that the current
    thread will at most wait for [t] (time value whose unit is [u]).
    Returns whether the condition was signaled; see
    {java java.util.concurrent.locks.Condition#await(long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the current thread does not hold the
                          associated lock
    @raise Java_exception if the thread is interrupted *)

val await_nanos : t -> java_long -> java_long
(** [await_nanos c n] is similar to [await c], except that the current
    thread will at most wait for [n] nanoseconds. Returns the duration of
    the wait, a negative value if the condition was not signaled; see
    {java java.util.concurrent.locks.Condition#awaitNanos(long)}.

    @raise Java_exception if the current thread does not hold the
                          associated lock
    @raise Java_exception if the thread is interrupted *)

val await_uninterruptibly : t -> unit
(** Similar to [await] except that the thread cannot be interrupted; see
    {java java.util.concurrent.locks.Condition#awaitUninterruptibly()}.

    @raise Java_exception if the current thread does not hold the
                          associated lock *)

val await_until : t -> java'util'Date java_extends -> bool
(** [await_until c d] waits until the date [d] is reached.
    Returns whether the date was actually reached; see
    {java java.util.concurrent.locks.Condition#awaitUntil()}.

    @raise Java_exception if the current thread does not hold the
                          associated lock
    @raise Java_exception if the thread is interrupted *)

val signal : t -> unit
(** Signals the condition, unblocking one waiting thread; see
    {java java.util.concurrent.locks.Condition#signal()}.

    @raise Java_exception if the current thread does not hold the
                          associated lock *)

val signal_all : t -> unit
(** Signals the condition, unblocking all waiting threads; see
    {java java.util.concurrent.locks.Condition#signalAll()}.

    @raise Java_exception if the current thread does not hold the
                          associated lock *)
