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

open Class'java'util'concurrent'RejectedExecutionHandler
open Class'java'util'concurrent'ThreadPoolExecutor
open Class'java'util'concurrent'TimeUnit

type t = _'ThreadPoolExecutor java_instance

external threadpoolexecutor_make : int32 -> int32 -> int64 -> TimeUnit.t -> RejectedExecutionHandler.t -> t =
  "ocamljava_threadpoolexecutor_make"

let make ~core_pool_size ~max_pool_size time timeunit rhe =
  threadpoolexecutor_make core_pool_size max_pool_size time timeunit rhe

let await_termination tpe time timeunit =
  Java.call "ThreadPoolExecutor.awaitTermination(long,TimeUnit)" tpe time timeunit

let get_active_count tpe =
  Java.call "ThreadPoolExecutor.getActiveCount()" tpe

let get_completed_task_count tpe =
  Java.call "ThreadPoolExecutor.getCompletedTaskCount()" tpe

let get_core_pool_size tpe =
  Java.call "ThreadPoolExecutor.getCorePoolSize()" tpe

let get_keep_alive_time tpe timeunit =
  Java.call "ThreadPoolExecutor.getKeepAliveTime(TimeUnit)" tpe timeunit

let get_largest_pool_size tpe =
  Java.call "ThreadPoolExecutor.getLargestPoolSize()" tpe

let get_maximum_pool_size tpe =
  Java.call "ThreadPoolExecutor.getMaximumPoolSize()" tpe

let get_pool_size tpe =
  Java.call "ThreadPoolExecutor.getPoolSize()" tpe

let get_rejected_execution_handler tpe =
  Java.call "ThreadPoolExecutor.getRejectedExecutionHandler()" tpe

let get_task_count tpe =
  Java.call "ThreadPoolExecutor.getTaskCount()" tpe

external invoke_all : t -> (unit -> 'a) list -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all"

external invoke_all_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all_time"

external invoke_any : t -> (unit -> 'a) list -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any"

external invoke_any_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any_time"

let is_shutdown tpe =
  Java.call "ThreadPoolExecutor.isShutdown()" tpe

let is_terminated tpe =
  Java.call "ThreadPoolExecutor.isTerminated()" tpe

let is_terminating tpe =
  Java.call "ThreadPoolExecutor.isTerminating()" tpe

let set_core_pool_size tpe sz =
  Java.call "ThreadPoolExecutor.setCorePoolSize(int)" tpe sz

let set_keep_alive_time tpe time timeunit =
  Java.call "ThreadPoolExecutor.setKeepAliveTime(long,TimeUnit)" tpe time timeunit

let set_maximum_pool_size tpe sz =
  Java.call "ThreadPoolExecutor.setMaximumPoolSize(int)" tpe sz

let set_rejected_execution_handler tpe reh =
  Java.call "ThreadPoolExecutor.setRejectedExecutionHandler(RejectedExecutionHandler)" tpe reh

let shutdown tpe =
  Java.call "ThreadPoolExecutor.shutdown()" tpe

external shutdown_now : t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_shutdown_now"

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_threadpoolexecutor_submit"


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
