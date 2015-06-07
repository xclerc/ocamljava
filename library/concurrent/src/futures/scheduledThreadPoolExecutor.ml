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
open Class'java'util'concurrent'ScheduledThreadPoolExecutor
open Class'java'util'concurrent'TimeUnit

type t = _'ScheduledThreadPoolExecutor java_instance

external scheduledthreadpoolexecutor_make : int32 -> RejectedExecutionHandler.t -> t =
  "ocamljava_scheduledthreadpoolexecutor_make"

let make ~core_pool_size rhe =
  scheduledthreadpoolexecutor_make core_pool_size rhe

let await_termination stpe time timeunit =
  Java.call "ScheduledThreadPoolExecutor.awaitTermination(long,TimeUnit)" stpe time timeunit

let get_active_count stpe =
  Java.call "ScheduledThreadPoolExecutor.getActiveCount()" stpe

let get_completed_task_count stpe =
  Java.call "ScheduledThreadPoolExecutor.getCompletedTaskCount()" stpe

let get_continue_existing_periodic_tasks_after_shutdown_policy stpe =
  Java.call "ScheduledThreadPoolExecutor.getContinueExistingPeriodicTasksAfterShutdownPolicy()" stpe

let get_core_pool_size stpe =
  Java.call "ScheduledThreadPoolExecutor.getCorePoolSize()" stpe

let get_execute_existing_delayed_tasks_after_shutdown_policy stpe =
  Java.call "ScheduledThreadPoolExecutor.getExecuteExistingDelayedTasksAfterShutdownPolicy()" stpe

let get_keep_alive_time stpe timeunit =
  Java.call "ScheduledThreadPoolExecutor.getKeepAliveTime(TimeUnit)" stpe timeunit

let get_largest_pool_size stpe =
  Java.call "ScheduledThreadPoolExecutor.getLargestPoolSize()" stpe

let get_maximum_pool_size stpe =
  Java.call "ScheduledThreadPoolExecutor.getMaximumPoolSize()" stpe

let get_pool_size stpe =
  Java.call "ScheduledThreadPoolExecutor.getPoolSize()" stpe

let get_rejected_execution_handler stpe =
  Java.call "ScheduledThreadPoolExecutor.getRejectedExecutionHandler()" stpe

let get_remove_on_cancel_policy stpe =
  Java.call "ScheduledThreadPoolExecutor.getRemoveOnCancelPolicy()" stpe

let get_task_count stpe =
  Java.call "ScheduledThreadPoolExecutor.getTaskCount()" stpe

external invoke_all : t -> (unit -> 'a) list -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all"

external invoke_all_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all_time"

external invoke_any : t -> (unit -> 'a) list -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any"

external invoke_any_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any_time"

let is_shutdown stpe =
  Java.call "ScheduledThreadPoolExecutor.isShutdown()" stpe

let is_terminated stpe =
  Java.call "ScheduledThreadPoolExecutor.isTerminated()" stpe

let is_terminating stpe =
  Java.call "ScheduledThreadPoolExecutor.isTerminating()" stpe

external schedule : t -> ('a -> 'b) -> 'a -> int64 -> TimeUnit.t -> 'b ScheduledFuture.t =
  "ocamljava_scheduledthreadpoolexecutor_schedule"

external schedule_at_fixed_rate : t -> ('a -> unit) -> 'a -> int64 -> int64 -> TimeUnit.t -> unit ScheduledFuture.t =
  "ocamljava_scheduledthreadpoolexecutor_schedule_at_fixed_rate" "ocamljava_scheduledthreadpoolexecutor_schedule_at_fixed_rate"

external schedule_with_fixed_delay : t -> ('a -> unit) -> 'a -> int64 -> int64 -> TimeUnit.t -> unit ScheduledFuture.t =
  "ocamljava_scheduledthreadpoolexecutor_schedule_with_fixed_delay" "ocamljava_scheduledthreadpoolexecutor_schedule_with_fixed_delay"

let set_continue_existing_periodic_tasks_after_shutdown_policy stpe b =
  Java.call "ScheduledThreadPoolExecutor.setContinueExistingPeriodicTasksAfterShutdownPolicy(boolean)"
    stpe b

let set_core_pool_size stpe sz =
  Java.call "ScheduledThreadPoolExecutor.setCorePoolSize(int)" stpe sz

let set_execute_existing_delayed_tasks_after_shutdown_policy stpe b =
  Java.call "ScheduledThreadPoolExecutor.setExecuteExistingDelayedTasksAfterShutdownPolicy(boolean)"
    stpe b

let set_keep_alive_time stpe time timeunit =
  Java.call "ScheduledThreadPoolExecutor.setKeepAliveTime(long,TimeUnit)" stpe time timeunit

let set_maximum_pool_size stpe sz =
  Java.call "ScheduledThreadPoolExecutor.setMaximumPoolSize(int)" stpe sz

let set_rejected_execution_handler stpe reh =
  Java.call "ScheduledThreadPoolExecutor.setRejectedExecutionHandler(RejectedExecutionHandler)"
    stpe reh

let set_remove_on_cancel_policy stpe b =
  Java.call "ScheduledThreadPoolExecutor.setRemoveOnCancelPolicy(boolean)" stpe b

let shutdown stpe =
  Java.call "ScheduledThreadPoolExecutor.shutdown()" stpe

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
