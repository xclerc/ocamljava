(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

external make : int32 -> int32 -> int64 -> TimeUnit.t -> RejectedExecutionHandler.t -> t =
  "ocamljava_threadpoolexecutor_make"

external await_termination : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_threadpoolexecutor_await_termination"

external get_active_count : t -> int32 =
  "ocamljava_threadpoolexecutor_get_active_count"

external get_completed_task_count : t -> int64 =
  "ocamljava_threadpoolexecutor_get_completed_task_count"

external get_core_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_core_pool_size"

external get_keep_alive_time : t -> TimeUnit.t -> int64 =
  "ocamljava_threadpoolexecutor_get_keep_alive_time"

external get_largest_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_largest_pool_size"

external get_maximum_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_maximum_pool_size"

external get_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_pool_size"

external get_rejected_execution_handler : t -> RejectedExecutionHandler.t =
  "ocamljava_threadpoolexecutor_get_rejected_execution_handler"

external get_task_count : t -> int64 =
  "ocamljava_threadpoolexecutor_get_task_count"

external invoke_all : t -> (unit -> 'a) list -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all"

external invoke_all_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all_time"

external invoke_any : t -> (unit -> 'a) list -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any"

external invoke_any_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any_time"

external is_shutdown : t -> bool =
  "ocamljava_threadpoolexecutor_is_shutdown"

external is_terminated : t -> bool =
  "ocamljava_threadpoolexecutor_is_terminated"

external is_terminating : t -> bool =
  "ocamljava_threadpoolexecutor_is_terminating"

external set_core_pool_size : t -> int32 -> unit =
  "ocamljava_threadpoolexecutor_set_core_pool_size"

external set_keep_alive_time : t -> int64 -> TimeUnit.t -> unit =
  "ocamljava_threadpoolexecutor_set_keep_alive_time"

external set_maximum_pool_size : t -> int32 -> unit =
  "ocamljava_threadpoolexecutor_set_maximum_pool_size"

external set_rejected_execution_handler : t -> RejectedExecutionHandler.t -> unit =
  "ocamljava_threadpoolexecutor_set_rejected_execution_handler"

external shutdown : t -> unit =
  "ocamljava_threadpoolexecutor_shutdown"

external shutdown_now : t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_shutdown_now"

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_threadpoolexecutor_submit"
