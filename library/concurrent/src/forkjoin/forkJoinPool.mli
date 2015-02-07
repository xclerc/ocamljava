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


type t
(** The type of thread pools to be used for fork/join computations. *)

external make : int32 -> bool -> t =
  "ocamljava_forkjoinpool_make"
(** [make p a] returns a new thread pool with parallelism [p] (target
    number of active threads in pool). If [a] is [true] a FIFO scheduling
    is used for tasks.

    Raises [Invalid_argument] if [p] is less than [1]. *)

external await_termination : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_forkjoinpool_await_termination"
(** Waits for pool termination, waiting at most wait for [t] (time value
    whose unit is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external get_active_thread_count : t -> int32 =
  "ocamljava_forkjoinpool_get_active_thread_count"
(** Returns an estimate of the number of active threads in the pool. *)

external get_async_mode : t -> bool =
  "ocamljava_forkjoinpool_get_async_mode"
(** Returns the asynchronous mode of the pool. *)

external get_parallelism : t -> int32 =
  "ocamljava_forkjoinpool_get_parallelism"
(** Returns the parallelism of the pool. *)

external get_pool_size : t -> int32 =
  "ocamljava_forkjoinpool_get_pool_size"
(** Returns the number of threads that have started and not terminated in
    the pool. *)

external get_queued_submission_count : t -> int32 =
  "ocamljava_forkjoinpool_get_queued_submission_count"
(** Returns an estimate of the number of tasks submitted (but unstarted)
    to the pool. *)

external get_queued_task_count : t -> int64 =
  "ocamljava_forkjoinpool_get_queued_task_count"
(** Returns an estimate of the number of tasks currently queued in the
    pool. *)

external get_running_thread_count : t -> int32 =
  "ocamljava_forkjoinpool_get_running_thread_count"
(** Returns an estimate of the number of threads in the pool. *)

external get_steal_count : t -> int64 =
  "ocamljava_forkjoinpool_get_steal_count"
(** Returns an estimate of the number of tasks stolen from one thread by
    another one in the pool. *)

external has_queued_submissions : t -> bool =
  "ocamljava_forkjoinpool_has_queued_submissions"
(** Tests whether there are submitted but unstarted tasks in the pool. *)

external is_quiescent : t -> bool =
  "ocamljava_forkjoinpool_is_quiescent"
(** Tests whether all threads in the pool are idle. *)

external is_shutdown : t -> bool =
  "ocamljava_forkjoinpool_is_shutdown"
(** Tests whether the pool has been shutdown. *)

external is_terminated : t -> bool =
  "ocamljava_forkjoinpool_is_terminated"
(** Tests whether shutdown is completed (including all tasks). *)

external is_terminating : t -> bool =
  "ocamljava_forkjoinpool_is_terminating"
(** Tests whether the pool is in the process shutdowning. *)

external shutdown : t -> unit =
  "ocamljava_forkjoinpool_shutdown"
(** Begins a shutdown, executing all submitted tasks, but refusing new
    tasks. *)

external shutdown_now : t -> unit =
  "ocamljava_forkjoinpool_shutdown_now"
(** Begins a shhutdown by cancelling all submitted tasks, and cancelling
    running tasks. *)
