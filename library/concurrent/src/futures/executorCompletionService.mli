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

(** Helper entities for thread pool executors. *)


type t = java'util'concurrent'ExecutorCompletionService java_instance
(** The type of completion services, providing utilities to wait for
    future completions. *)

val make : ThreadPoolExecutor.t -> t
(** Returns a new completion service based on the passed thread pool. *)

val poll : t -> 'a Future.t option
(** Returns (and removes from the service) a completed task if any, or
    returns [None]. *)

val poll_time : t -> java_long -> TimeUnit.t -> 'a Future.t option
(** [poll_time s t u] is similar to [pool s], except that the current
    thread will at most wait for [t] (time value whose unit is [u]).

    @raise Java_exception if the thread is interrupted *)

val submit : t -> ('a -> 'b) -> 'a -> 'b Future.t
(** Same as {!ThreadPoolExecutor.submit}. *)

val take : t -> 'a Future.t
(** Waits for a task to complete, and returns it.

    @raise Java_exception if the thread is interrupted *)
