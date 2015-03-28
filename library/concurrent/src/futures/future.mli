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

(** Computations run in background. *)

type 'a t = java'util'concurrent'Future java_instance
(** The type of futures, that are computations run in background. *)
    
val cancel : 'a t -> bool -> bool
(** [cancel f i] attemps to cancel future [f], [i] indicating whether to
    interrupt the computation if already started. Returns whether the
    future was cancelled; see
    {java java.util.concurrent.Future.cancel(boolean)}. *)

val get : 'a t -> 'a
(** Waits for the computation to complete, and returns its result; see
    {java java.util.concurrent.Future.get()}.

    @raise Java_exception if the thread is interrupted
    @raise Java_exception is the computation raised an uncaught exception
    @raise Java_exception if the computation was cancelled *)

val get_time : 'a t -> java_long -> TimeUnit.t -> 'a
(** [get_time f t u] is similar to [get f], except that the current
    thread will at most wait for [t] (time value whose unit is [u]); see
    {java java.util.concurrent.Future.get(long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted
    @raise Java_exception is the computation raised an uncaught exception
    @raise Java_exception if the computation was cancelled
    @raise Java_exception if time has elapsed without completion *)

val is_cancelled : 'a t -> bool
(** Tests whether the task was cancelled before completion; see
    {java java.util.concurrent.Future.isCancelled()}. *)

val is_done : 'a t -> bool
(** Tests whether the computation is completed; see
    {java java.util.concurrent.Future.isDone()}. *)
