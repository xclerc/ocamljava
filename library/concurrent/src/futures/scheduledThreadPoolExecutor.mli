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

(** Thread pools for scheduled futures. *)


type t = java'util'concurrent'ScheduledThreadPoolExecutor java_instance
(** The type of thread pools to be used for scheduled futures. *)

val make : core_pool_size:java_int -> RejectedExecutionHandler.t -> t
(** [make core_pool_size:cps reh] returns a new thread pool with:
    - [cps] as its core pool size (number of threads kept in the pool,
      even if idle);
    - [reh] policy for blocked computations.

    @raise Java_exception if [cps] is negative *)

val await_termination : t -> java_long -> TimeUnit.t -> bool
(** Same as {!ThreadPoolExecutor.await_termination}. *)

val get_active_count : t -> java_int
(** Same as {!ThreadPoolExecutor.get_active_count}. *)

val get_completed_task_count : t -> java_long
(** Same as {!ThreadPoolExecutor.get_completed_task_count}. *)

val get_continue_existing_periodic_tasks_after_shutdown_policy : t -> bool
(** Tests whether periodic tasks should continue execution after
    shutdown; see
    {java java.util.concurrent.ThreadPoolExecutor#getContinueExistingPeriodicTasksAfterShutdownPolicy()}. *)

val get_core_pool_size : t -> java_int
(** Same as {!ThreadPoolExecutor.get_core_pool_size}. *)

val get_execute_existing_delayed_tasks_after_shutdown_policy : t -> bool
(** Tests whether delayed tasks should continue execution after
    shutdown; see
    {java java.util.concurrent.ThreadPoolExecutor#getExecuteExistingDelayedTasksAfterShutdownPolicy()}. *)

val get_keep_alive_time : t -> TimeUnit.t -> java_long
(** Same as {!ThreadPoolExecutor.get_keep_alive_time}. *)

val get_largest_pool_size : t -> java_int
(** Same as {!ThreadPoolExecutor.get_largest_pool_size}. *)

val get_maximum_pool_size : t -> java_int
(** Same as {!ThreadPoolExecutor.get_maximum_pool_size}. *)

val get_pool_size : t -> java_int
(** Same as {!ThreadPoolExecutor.get_pool_size}. *)

val get_rejected_execution_handler : t -> RejectedExecutionHandler.t
(** Same as {!ThreadPoolExecutor.get_rejected_execution_handler}. *)

val get_remove_on_cancel_policy : t -> bool
(** Tests whether tasks should be removed when cancelled; see
    {java java.util.concurrent.ThreadPoolExecutor#getRemoveOnCancelPolicy()}. *)

val get_task_count : t -> java_long
(** Same as {!ThreadPoolExecutor.get_task_count}. *)

val invoke_all : t -> (unit -> 'a) list -> 'a Future.t list
(** Same as {!ThreadPoolExecutor.invoke_all}. *)

val invoke_all_time : t -> (unit -> 'a) list -> java_long -> TimeUnit.t -> 'a Future.t list
(** Same as {!ThreadPoolExecutor.invoke_all_time}. *)

val invoke_any : t -> (unit -> 'a) list -> 'a
(** Same as {!ThreadPoolExecutor.invoke_any}. *)

val invoke_any_time : t -> (unit -> 'a) list -> java_long -> TimeUnit.t -> 'a
(** Same as {!ThreadPoolExecutor.invoke_any_time}. *)

val is_shutdown : t -> bool
(** Same as {!ThreadPoolExecutor.is_shutdown}. *)

val is_terminated : t -> bool
(** Same as {!ThreadPoolExecutor.is_terminated}. *)

val is_terminating : t -> bool
(** Same as {!ThreadPoolExecutor.is_terminating}. *)

val schedule : t -> ('a -> 'b) -> 'a -> java_long -> TimeUnit.t -> 'b ScheduledFuture.t
(** [schedule p f x t u] is similar to [submit p f x], except that the
    evaluation of [f x] with start after [t] (time value whose unit is
    [u]).

    @raise Java_exception if pool limits are reached *)

val schedule_at_fixed_rate : t -> ('a -> unit) -> 'a -> java_long -> java_long -> TimeUnit.t -> unit ScheduledFuture.t
(** [schedule_at_fixed_rate p f x t d u] is similar to [schedule p f x t u],
    except that [f x] will be re-evaluated at [t + d], [t + 2 * d], {i etc.}

    @raise Java_exception if pool limits are reached
    @raise Java_exception if [d] is negative *)

val schedule_with_fixed_delay : t -> ('a -> unit) -> 'a -> java_long -> java_long -> TimeUnit.t -> unit ScheduledFuture.t
(** [schedule_with_fixed_delay p f x t d u] is similar to [schedule p f x t u],
    except that [f x] will be repeatedly re-evaluated, respecting a delay
    of [d] between the end of one execution and the beginning of the next
    one.

    @raise Java_exception if pool limits are reached
    @raise Java_exception if [d] is negative *)

val set_continue_existing_periodic_tasks_after_shutdown_policy : t -> bool -> unit
(** Changes whether periodic tasks should continue execution after
    shutdown; see
    {java java.util.concurrent.ThreadPoolExecutor#setContinueExistingPeriodicTasksAfterShutdownPolicy(boolean)}. *)

val set_core_pool_size : t -> java_int -> unit
(** Same as {!ThreadPoolExecutor.set_core_pool_size}. *)

val set_execute_existing_delayed_tasks_after_shutdown_policy : t -> bool -> unit
(** changes whether delayed tasks should continue execution after
    shutdown; see
    {java java.util.concurrent.ThreadPoolExecutor#setExecuteExistingDelayedTasksAfterShutdownPolicy(boolean)}. *)

val set_keep_alive_time : t -> java_long -> TimeUnit.t -> unit
(** Same as {!ThreadPoolExecutor.set_keep_alive_time}. *)

val set_maximum_pool_size : t -> java_int -> unit
(** Same as {!ThreadPoolExecutor.set_maximum_pool_size}. *)

val set_rejected_execution_handler : t -> RejectedExecutionHandler.t -> unit
(** Same as {!ThreadPoolExecutor.set_rejected_execution_handler}. *)

val set_remove_on_cancel_policy : t -> bool -> unit
(** Changes whether tasks should be removed when cancelled. *)

val shutdown : t -> unit
(** Same as {!ThreadPoolExecutor.shutdown}. *)

val shutdown_now : t -> 'a Future.t list
(** Same as {!ThreadPoolExecutor.shutdown_now}. *)

val submit : t -> ('a -> 'b) -> 'a -> 'b Future.t
(** Same as {!ThreadPoolExecutor.submit}. *)


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
