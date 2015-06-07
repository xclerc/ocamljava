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

(** Thread pools for fork/join computations. *)


type t = java'util'concurrent'ForkJoinPool java_instance
(** The type of thread pools to be used for fork/join computations. *)

val make : ?parallelism:java_int -> bool -> t
(** [make parallelism:p async] returns a new thread pool with parallelism [p]
    (target number of active threads in pool). If [async] is [true], then a
    FIFO scheduling is used for tasks.
    See {java java.util.concurrent.ForkJoinPool#ForkJoinPool(int)}.

    @raise Java_exception if [p] is less than [1] *)

val await_termination : t -> java_long -> TimeUnit.t -> bool
(** Waits for pool termination, waiting at most wait for [t] (time value
    whose unit is [u]); see
    {java java.util.concurrent.ForkJoinPool#awaitTermination(long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted *)

val get_active_thread_count : t -> java_int
(** Returns an estimate of the number of active threads in the pool; see
    {java java.util.concurrent.ForkJoinPool#getActiveThreadCount()}. *)

val get_async_mode : t -> bool
(** Returns the asynchronous mode of the pool; see
    {java java.util.concurrent.ForkJoinPool#getAsyncMode()}. *)

val get_parallelism : t -> java_int
(** Returns the parallelism of the pool; see
    {java java.util.concurrent.ForkJoinPool#getParallelism()}. *)

val get_pool_size : t -> java_int
(** Returns the number of threads that have started and not terminated in
    the pool; see
    {java java.util.concurrent.ForkJoinPool#getPoolSize()}. *)

val get_queued_submission_count : t -> java_int
(** Returns an estimate of the number of tasks submitted (but unstarted)
    to the pool; see
    {java java.util.concurrent.ForkJoinPool#getQueuedSubmissionCount()}. *)

val get_queued_task_count : t -> java_long
(** Returns an estimate of the number of tasks currently queued in the
    pool; see
    {java java.util.concurrent.ForkJoinPool#getQueuedTaskCount()}. *)

val get_running_thread_count : t -> java_int
(** Returns an estimate of the number of threads in the pool; see
    {java java.util.concurrent.ForkJoinPool#getRunningThreadCount()}. *)

val get_steal_count : t -> java_long
(** Returns an estimate of the number of tasks stolen from one thread by
    another one in the pool; see
    {java java.util.concurrent.ForkJoinPool#getStealCount()}. *)

val has_queued_submissions : t -> bool
(** Tests whether there are submitted but unstarted tasks in the pool; see
    {java java.util.concurrent.ForkJoinPool#hasQueuedSubmissions()}. *)

val is_quiescent : t -> bool
(** Tests whether all threads in the pool are idle; see
    {java java.util.concurrent.ForkJoinPool#isQuiescent()}. *)

val is_shutdown : t -> bool
(** Tests whether the pool has been shutdown; see
    {java java.util.concurrent.ForkJoinPool#isShutdown()}. *)

val is_terminated : t -> bool
(** Tests whether shutdown is completed (including all tasks); see
    {java java.util.concurrent.ForkJoinPool#isTerminated()}. *)

val is_terminating : t -> bool
(** Tests whether the pool is in the process shutdowning; see
    {java java.util.concurrent.ForkJoinPool#isTerminating()}. *)

val shutdown : t -> unit
(** Begins a shutdown, executing all submitted tasks, but refusing new
    tasks; see {java java.util.concurrent.ForkJoinPool#shutdown()}. *)

val shutdown_now : t -> unit
(** Begins a shhutdown by cancelling all submitted tasks, and cancelling
    running tasks; see
    {java java.util.concurrent.ForkJoinPool#shutdownNow()}. *)


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
