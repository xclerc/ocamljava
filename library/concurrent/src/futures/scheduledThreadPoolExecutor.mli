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

(** Thread pools for scheduled futures. *)


type t
(** The type of thread pools to be used for scheduled futures. *)

external make : int32 -> RejectedExecutionHandler.t -> t =
  "ocamljava_scheduledthreadpoolexecutor_make"
(** [make cps reh] returns a new thread pool with:
    - [cps] as its core pool size (number of threads kept in the pool,
      even if idle);
    - [reh] policy for blocked computations.

    Raises [Invalid_argument] if [cps] is negative. *)

external await_termination : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_threadpoolexecutor_await_termination"
(** Same as {!ThreadPoolExecutor.await_termination}. *)

external get_active_count : t -> int32 =
  "ocamljava_threadpoolexecutor_get_active_count"
(** Same as {!ThreadPoolExecutor.get_active_count}. *)

external get_completed_task_count : t -> int64 =
  "ocamljava_threadpoolexecutor_get_completed_task_count"
(** Same as {!ThreadPoolExecutor.get_completed_task_count}. *)

external get_continue_existing_periodic_tasks_after_shutdown_policy : t -> bool =
  "ocamljava_scheduledthreadpoolexecutor_get_continue_existing_periodic_tasks_after_shutdown_policy"
(** Tests whether periodic tasks should continue execution after
    shutdown. *)

external get_core_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_core_pool_size"
(** Same as {!ThreadPoolExecutor.get_core_pool_size}. *)

external get_execute_existing_delayed_tasks_after_shutdown_policy : t -> bool =
  "ocamljava_scheduledthreadpoolexecutor_get_execute_existing_delayed_tasks_after_shutdown_policy"
(** Tests whether delayed tasks should continue execution after
    shutdown. *)

external get_keep_alive_time : t -> TimeUnit.t -> int64 =
  "ocamljava_threadpoolexecutor_get_keep_alive_time"
(** Same as {!ThreadPoolExecutor.get_keep_alive_time}. *)

external get_largest_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_largest_pool_size"
(** Same as {!ThreadPoolExecutor.get_largest_pool_size}. *)

external get_maximum_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_maximum_pool_size"
(** Same as {!ThreadPoolExecutor.get_maximum_pool_size}. *)

external get_pool_size : t -> int32 =
  "ocamljava_threadpoolexecutor_get_pool_size"
(** Same as {!ThreadPoolExecutor.get_pool_size}. *)

external get_rejected_execution_handler : t -> RejectedExecutionHandler.t =
  "ocamljava_threadpoolexecutor_get_rejected_execution_handler"
(** Same as {!ThreadPoolExecutor.get_rejected_execution_handler}. *)

external get_remove_on_cancel_policy : t -> bool =
  "ocamljava_scheduledthreadpoolexecutor_get_remove_on_cancel_policy"
(** Tests whether tasks should be removed when cancelled. *)

external get_task_count : t -> int64 =
  "ocamljava_threadpoolexecutor_get_task_count"
(** Same as {!ThreadPoolExecutor.get_task_count}. *)

external invoke_all : t -> (unit -> 'a) list -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all"
(** Same as {!ThreadPoolExecutor.invoke_all}. *)

external invoke_all_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_invoke_all_time"
(** Same as {!ThreadPoolExecutor.invoke_all_time}. *)

external invoke_any : t -> (unit -> 'a) list -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any"
(** Same as {!ThreadPoolExecutor.invoke_any}. *)

external invoke_any_time : t -> (unit -> 'a) list -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_threadpoolexecutor_invoke_any_time"
(** Same as {!ThreadPoolExecutor.invoke_any_time}. *)

external is_shutdown : t -> bool =
  "ocamljava_threadpoolexecutor_is_shutdown"
(** Same as {!ThreadPoolExecutor.is_shutdown}. *)

external is_terminated : t -> bool =
  "ocamljava_threadpoolexecutor_is_terminated"
(** Same as {!ThreadPoolExecutor.is_terminated}. *)

external is_terminating : t -> bool =
  "ocamljava_threadpoolexecutor_is_terminating"
(** Same as {!ThreadPoolExecutor.is_terminating}. *)

external schedule : t -> ('a -> 'b) -> 'a -> int64 -> TimeUnit.t -> 'b ScheduledFuture.t =
  "ocamljava_scheduledthreadpoolexecutor_schedule"
(** [schedule p f x t u] is similar to [submit p f x], except that the
    evaluation of [f x] with start after [t] (time value whose unit is
    [u]).

    Raises [Failure] if pool limits are reached. *)

external schedule_at_fixed_rate : t -> ('a -> unit) -> 'a -> int64 -> int64 -> TimeUnit.t -> unit ScheduledFuture.t =
  "ocamljava_scheduledthreadpoolexecutor_schedule_at_fixed_rate" "ocamljava_scheduledthreadpoolexecutor_schedule_at_fixed_rate"
(** [schedule_at_fixed_rate p f x t d u] is similar to [schedule p f x t u],
    except that [f x] will be re-evaluated at [t + d], [t + 2 * d], {i etc.}

    Raises [Failure] if pool limits are reached.

    Raises [Invalid_argument] if [d] is negative. *)

external schedule_with_fixed_delay : t -> ('a -> unit) -> 'a -> int64 -> int64 -> TimeUnit.t -> unit ScheduledFuture.t =
  "ocamljava_scheduledthreadpoolexecutor_schedule_with_fixed_delay" "ocamljava_scheduledthreadpoolexecutor_schedule_with_fixed_delay"
(** [schedule_with_fixed_delay p f x t d u] is similar to [schedule p f x t u],
    except that [f x] will be repeatedly re-evaluated, respecting a delay
    of [d] between the end of one execution and the beginning of the next
    one.

    Raises [Failure] if pool limits are reached.

    Raises [Invalid_argument] if [d] is negative. *)

external set_continue_existing_periodic_tasks_after_shutdown_policy : t -> bool -> unit =
  "ocamljava_scheduledthreadpoolexecutor_set_continue_existing_periodic_tasks_after_shutdown_policy"
(** Changes whether periodic tasks should continue execution after
    shutdown. *)

external set_core_pool_size : t -> int32 -> unit =
  "ocamljava_threadpoolexecutor_set_core_pool_size"
(** Same as {!ThreadPoolExecutor.set_core_pool_size}. *)

external set_execute_existing_delayed_tasks_after_shutdown_policy : t -> bool -> unit =
  "ocamljava_scheduledthreadpoolexecutor_set_execute_existing_delayed_tasks_after_shutdown_policy"
(** changes whether delayed tasks should continue execution after
    shutdown. *)

external set_keep_alive_time : t -> int64 -> TimeUnit.t -> unit =
  "ocamljava_threadpoolexecutor_set_keep_alive_time"
(** Same as {!ThreadPoolExecutor.set_keep_alive_time}. *)

external set_maximum_pool_size : t -> int32 -> unit =
  "ocamljava_threadpoolexecutor_set_maximum_pool_size"
(** Same as {!ThreadPoolExecutor.set_maximum_pool_size}. *)

external set_rejected_execution_handler : t -> RejectedExecutionHandler.t -> unit =
  "ocamljava_threadpoolexecutor_set_rejected_execution_handler"
(** Same as {!ThreadPoolExecutor.set_rejected_execution_handler}. *)

external set_remove_on_cancel_policy : t -> bool -> unit =
  "ocamljava_scheduledthreadpoolexecutor_set_remove_on_cancel_policy"
(** Changes whether tasks should be removed when cancelled. *)

external shutdown : t -> unit =
  "ocamljava_threadpoolexecutor_shutdown"
(** Same as {!ThreadPoolExecutor.shutdown}. *)

external shutdown_now : t -> 'a Future.t list =
  "ocamljava_threadpoolexecutor_shutdown_now"
(** Same as {!ThreadPoolExecutor.shutdown_now}. *)

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_threadpoolexecutor_submit"
(** Same as {!ThreadPoolExecutor.submit}. *)
