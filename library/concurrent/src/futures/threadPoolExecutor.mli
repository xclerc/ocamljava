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


type t
(** The type of thread pools to be used for futures. *)

external make : int32 -> int32 -> int64 -> TimeUnit.t -> RejectedExecutionHandler.t -> t =
  "ocamljava_threadpoolexecutor_make"
(** [make cps mps kat u reh] returns a new thread pool with:
    - [cps] as its core pool size (number of threads kept in the pool,
      even if idle);
    - [mps] as its maximum pool size (maximum number of threads in the
      pool);
    - [kat] keep alive time (how long to keep alive idle threads not in
      the core);
    - [u] time unit for [kat];
    - [reh] policy for blocked computations.

    Raises [Invalid_argument] if a size is negative. *)

external await_termination : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_threadpoolexecutor_await_termination"
(** Waits for pool termination, waiting at most wait for [t] (time value
    whose unit is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external get_active_count : t -> int32 =
  "ocamljava_threadpoolexecutor_get_active_count"
(** Returns an estimate of the number of active threads in the pool. *)

external get_completed_task_count : t -> int64 =
  "ocamljava_threadpoolexecutor_get_completed_task_count"
(** Returns an estimate of the number of completed computations in the
    pool. *)

external get_core_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_core_pool_size"
(** Returns the core size of the pool. *)

external get_keep_alive_time : t -> TimeUnit.t -> int64 =
  "ocamljava_threadpoolexecutor_get_keep_alive_time"
(** Returns the keep alive time for thread outside of the core.*)

external get_largest_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_largest_pool_size"
(** Returns the largest size reached by the pool. *)

external get_maximum_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_maximum_pool_size"
(** Returns the maximum size of the pool. *)

external get_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_pool_size"
(** Returns an estimate of the current pool size. *)

external get_rejected_execution_handler : t -> RejectedExecutionHandler.t =
  "ocamljava_threadpoolexecutor_get_rejected_execution_handler"
(** Returns the policy for blocked computations. *)

external get_task_count : t -> int64 =
  "ocamljava_threadpoolexecutor_get_task_count"
(** Returns an estimate of number of task that have been submitted for
    execution. *)

external invoke_all : t -> (unit -> 'a) list -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all"
(** [invoke_all p l] submits all functions from [l] to pool [p], and then
    waits for the completion of all functions. Returns the list of
    created futures; it is guaranteed that all futures have completed.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external invoke_all_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all_time"
(** [invoke_all_time p l t u] is similar to [invoke_all p l], except that
    the current thread will at most wait for [t] (time value whose unit
    is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external invoke_any : t -> (unit -> 'a) list -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any"
(** [invoke_any p l] submits all function from [l] to pool [p], and then
    waits for the completion of a function. Returns the value computed by
    the first function completing its execution.

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Failure] if no function completes without raising an
    exception. *)

external invoke_any_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any_time"
(** [invoke_any_time p l t u] is similar to [invoke_any p l], except that
    the current thread will at most wait for [t] (time value whose unit
    is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Failure] if no function completes without raising an
    exception.

    Raises [Runtime.Timeout] f time has elapsed without any function
    completing its execution. *)

external is_shutdown : t -> bool =
  "ocamljava_threadpoolexecutor_is_shutdown"
(** Tests whether the pool has been shutdown. *)

external is_terminated : t -> bool =
  "ocamljava_threadpoolexecutor_is_terminated"
(** Tests whether the pool has been shutdown. *)

external is_terminating : t -> bool =
  "ocamljava_threadpoolexecutor_is_terminating"
(** Tests whether the pool is in the process shutdowning. *)

external set_core_pool_size : t -> int32 -> unit =
  "ocamljava_threadpoolexecutor_set_core_pool_size"
(** Changes the core size of the pool (number of threads kept in the
    pool, even if idle).

    Raises [Invalid_argument] if parameter is negative. *)

external set_keep_alive_time : t -> int64 -> TimeUnit.t -> unit =
  "ocamljava_threadpoolexecutor_set_keep_alive_time"
(** Changes the keep alive time (how long to keep alive idle threads not
    in the core).

    Raises [Invalid_argument] if parameter is negative. *)

external set_maximum_pool_size : t -> int32 -> unit =
  "ocamljava_threadpoolexecutor_set_maximum_pool_size"
(** Changes the maximum pool size (maximum number of threads in the
    pool).

    Raises [Invalid_argument] if parameter is negative. *)

external set_rejected_execution_handler : t -> RejectedExecutionHandler.t -> unit =
  "ocamljava_threadpoolexecutor_set_rejected_execution_handler"
(** Changes the policy for blocked computations. *)

external shutdown : t -> unit =
  "ocamljava_threadpoolexecutor_shutdown"
(** Begins a shutdown, executing all submitted tasks, but refusing new
    tasks. *)

external shutdown_now : t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_shutdown_now"
(** Begins a shhutdown by cancelling all submitted tasks, and cancelling
    running tasks. *)

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_threadpoolexecutor_submit"
(** [submit p f x] submits to [p] and returns a future computing [f x].

    Raises [Failure] if pool limits are reached. *)
