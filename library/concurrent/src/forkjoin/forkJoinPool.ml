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

type t

external make : int32 -> bool -> t =
  "ocamljava_forkjoinpool_make"

external await_termination : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_forkjoinpool_await_termination"

external get_active_thread_count : t -> int32 =
  "ocamljava_forkjoinpool_get_active_thread_count"

external get_async_mode : t -> bool =
  "ocamljava_forkjoinpool_get_async_mode"

external get_parallelism : t -> int32 =
  "ocamljava_forkjoinpool_get_parallelism"

external get_pool_size : t -> int32 =
  "ocamljava_forkjoinpool_get_pool_size"

external get_queued_submission_count : t -> int32 =
  "ocamljava_forkjoinpool_get_queued_submission_count"

external get_queued_task_count : t -> int64 =
  "ocamljava_forkjoinpool_get_queued_task_count"

external get_running_thread_count : t -> int32 =
  "ocamljava_forkjoinpool_get_running_thread_count"

external get_steal_count : t -> int64 =
  "ocamljava_forkjoinpool_get_steal_count"

external has_queued_submissions : t -> bool =
  "ocamljava_forkjoinpool_has_queued_submissions"

external is_quiescent : t -> bool =
  "ocamljava_forkjoinpool_is_quiescent"

external is_shutdown : t -> bool =
  "ocamljava_forkjoinpool_is_shutdown"

external is_terminated : t -> bool =
  "ocamljava_forkjoinpool_is_terminated"

external is_terminating : t -> bool =
  "ocamljava_forkjoinpool_is_terminating"

external shutdown : t -> unit =
  "ocamljava_forkjoinpool_shutdown"

external shutdown_now : t -> unit =
  "ocamljava_forkjoinpool_shutdown_now"
