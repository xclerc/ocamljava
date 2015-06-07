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

(** Thread pools for futures. *)


type t = java'util'concurrent'ThreadPoolExecutor java_instance
(** The type of thread pools to be used for futures. *)

val make : core_pool_size:java_int -> max_pool_size:java_int -> java_long -> TimeUnit.t -> RejectedExecutionHandler.t -> t
(** [make core_pool_size:cps max_pool_size:mps kat u reh] returns a new thread pool with:
    - [cps] as its core pool size (number of threads kept in the pool,
      even if idle);
    - [mps] as its maximum pool size (maximum number of threads in the
      pool);
    - [kat] keep alive time (how long to keep alive idle threads not in
      the core);
    - [u] time unit for [kat];
    - [reh] policy for blocked computations.

    @raise Java_exception if a size is negative *)

val await_termination : t -> java_long -> TimeUnit.t -> bool
(** Waits for pool termination, waiting at most wait for [t] (time value
    whose unit is [u]); see {java java.util.concurrent.ThreadPoolExecutor#awaitTermination(long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted *)

val get_active_count : t -> java_int
(** Returns an estimate of the number of active threads in the pool; see
    {java java.util.concurrent.ThreadPoolExecutor#getActiveCount()}. *)

val get_completed_task_count : t -> java_long
(** Returns an estimate of the number of completed computations in the
    pool; see {java java.util.concurrent.ThreadPoolExecutor#getCompletedTaskCount()}. *)

val get_core_pool_size : t -> java_int
(** Returns the core size of the pool; see
    {java java.util.concurrent.ThreadPoolExecutor#getCorePoolSize()}. *)

val get_keep_alive_time : t -> TimeUnit.t -> java_long
(** Returns the keep alive time for thread outside of the core; see
    {java java.util.concurrent.ThreadPoolExecutor#getKeepAliveTime(java.util.concurrent.TimeUnit)}.*)

val get_largest_pool_size : t -> java_int
(** Returns the largest size reached by the pool; see
    {java java.util.concurrent.ThreadPoolExecutor#getLargestPoolSize()}. *)

val get_maximum_pool_size : t -> java_int
(** Returns the maximum size of the pool; see
    {java java.util.concurrent.ThreadPoolExecutor#getMaximumPoolSize()}. *)

val get_pool_size : t -> java_int
(** Returns an estimate of the current pool size; see
    {java java.util.concurrent.ThreadPoolExecutor#getPoolSize()}. *)

val get_rejected_execution_handler : t -> RejectedExecutionHandler.t
(** Returns the policy for blocked computations; see
    {java java.util.concurrent.ThreadPoolExecutor#getRejectedExecutionHandler()}. *)

val get_task_count : t -> java_long
(** Returns an estimate of number of task that have been submitted for
    execution; see {java java.util.concurrent.ThreadPoolExecutor#getTaskCount()}. *)

val invoke_all : t -> (unit -> 'a) list -> 'a Future.t list
(** [invoke_all p l] submits all functions from [l] to pool [p], and then
    waits for the completion of all functions. Returns the list of
    created futures; it is guaranteed that all futures have completed.

    @raise Java_exception if the thread is interrupted *)

val invoke_all_time : t -> (unit -> 'a) list -> java_long -> TimeUnit.t -> 'a Future.t list
(** [invoke_all_time p l t u] is similar to [invoke_all p l], except that
    the current thread will at most wait for [t] (time value whose unit
    is [u]).

    @raise Java_exception if the thread is interrupted *)

val invoke_any : t -> (unit -> 'a) list -> 'a
(** [invoke_any p l] submits all function from [l] to pool [p], and then
    waits for the completion of a function. Returns the value computed by
    the first function completing its execution.

    @raise Java_exception if the thread is interrupted
    @raise Java_exception if no function completes without raising an
                          exception *)

val invoke_any_time : t -> (unit -> 'a) list -> java_long -> TimeUnit.t -> 'a
(** [invoke_any_time p l t u] is similar to [invoke_any p l], except that
    the current thread will at most wait for [t] (time value whose unit
    is [u]).

    @raise Java_exception if the thread is interrupted
    @raise Java_exception f time has elapsed without any function
                          completing its execution
    @raise Java_exception if no function completes without raising an
                          exception *)

val is_shutdown : t -> bool
(** Tests whether the pool has been shutdown; see
    {java java.util.concurrent.ThreadPoolExecutor#isShutdown()}. *)

val is_terminated : t -> bool
(** Tests whether the pool has been terminated; see
    {java java.util.concurrent.ThreadPoolExecutor#isTerminated()}. *)

val is_terminating : t -> bool
(** Tests whether the pool is in the process of terminating; see
    {java java.util.concurrent.ThreadPoolExecutor#isTerminating()}. *)

val set_core_pool_size : t -> java_int -> unit
(** Changes the core size of the pool (number of threads kept in the
    pool, even if idle); see
    {java java.util.concurrent.ThreadPoolExecutor#setCorePoolSize(int)}. *)

val set_keep_alive_time : t -> java_long -> TimeUnit.t -> unit
(** Changes the keep alive time (how long to keep alive idle threads not
    in the core); see
    {java java.util.concurrent.ThreadPoolExecutor#setKeepAliveTime(long, java.util.concurrent.TimeUnit)}. *)

val set_maximum_pool_size : t -> java_int -> unit
(** Changes the maximum pool size (maximum number of threads in the
    pool); see
    {java java.util.concurrent.ThreadPoolExecutor#setMaximumPoolSize(int)}. *)

val set_rejected_execution_handler : t -> RejectedExecutionHandler.t -> unit
(** Changes the policy for blocked computations; see
    {java java.util.concurrent.ThreadPoolExecutor#setRejectedExecutionHandler(java.util.concurrent.RejectedExecutionHandler)}. *)

val shutdown : t -> unit
(** Begins a shutdown, executing all submitted tasks, but refusing new
    tasks; see
    {java java.util.concurrent.ThreadPoolExecutor#shutdown()}. *)

val shutdown_now : t -> 'a Future.t list
(** Begins a shhutdown by cancelling all submitted tasks, and cancelling
    running tasks; see
    {java java.util.concurrent.ThreadPoolExecutor#shutdownNow()}. *)

val submit : t -> ('a -> 'b) -> 'a -> 'b Future.t
(** [submit p f x] submits to [p] and returns a future computing [f x].

    @raise Java_exception if pool limits are reached *)


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
