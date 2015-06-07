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

open Class'java'util'concurrent'ForkJoinPool
open Class'java'util'concurrent'TimeUnit

type t = _'ForkJoinPool java_instance

external ocamljava_forkjoinpool_make : int32 -> bool -> t =
  "ocamljava_forkjoinpool_make"

let default_parallelism () =
  Java.call "Runtime.availableProcessors()"
    (Java.call "Runtime.getRuntime()" ())

let make ?(parallelism = default_parallelism ()) a =
  ocamljava_forkjoinpool_make parallelism a

let await_termination pool time timeunit =
  Java.call "ForkJoinPool.awaitTermination(long,TimeUnit)" pool time timeunit

let get_active_thread_count pool =
  Java.call "ForkJoinPool.getActiveThreadCount()" pool

let get_async_mode pool =
  Java.call "ForkJoinPool.getAsyncMode()" pool

let get_parallelism pool =
  Java.call "ForkJoinPool.getParallelism()" pool

let get_pool_size pool =
  Java.call "ForkJoinPool.getPoolSize()" pool

let get_queued_submission_count pool =
  Java.call "ForkJoinPool.getQueuedSubmissionCount()" pool

let get_queued_task_count pool =
  Java.call "ForkJoinPool.getQueuedTaskCount()" pool

let get_running_thread_count pool =
  Java.call "ForkJoinPool.getRunningThreadCount()" pool

let get_steal_count pool =
  Java.call "ForkJoinPool.getStealCount()" pool

let has_queued_submissions pool =
  Java.call "ForkJoinPool.hasQueuedSubmissions()" pool

let is_quiescent pool =
  Java.call "ForkJoinPool.isQuiescent()" pool

let is_shutdown pool =
  Java.call "ForkJoinPool.isShutdown()" pool

let is_terminated pool =
  Java.call "ForkJoinPool.isTerminated()" pool

let is_terminating pool =
  Java.call "ForkJoinPool.isTerminating()" pool

let shutdown pool =
  Java.call "ForkJoinPool.shutdown()" pool

let shutdown_now pool =
  Java.exec "ForkJoinPool.shutdownNow()" pool


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
