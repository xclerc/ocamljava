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

(** Helper entities for thread pool executors. *)


type t
(** The type of completion services, providing utilities to wait for
    future completions. *)

external make : ThreadPoolExecutor.t -> t =
  "ocamljava_executorcompletionservice_make"
(** Returns a new completion service based on the passed thread pool. *)

external poll : t -> 'a Future.t option =
  "ocamljava_executorcompletionservice_poll"
(** Returns (and removes from the service) a completed task if any, or
    returns [None]. *)

external poll_time : t -> int64 -> TimeUnit.t -> 'a Future.t option =
  "ocamljava_executorcompletionservice_poll_time"
(** [poll_time s t u] is similar to [pool s], except that the current
    thread will at most wait for [t] (time value whose unit is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_executorcompletionservice_submit"
(** Same as {!ThreadPoolExecutor.submit}. *)

external take : t -> 'a Future.t =
  "ocamljava_executorcompletionservice_take"
(** Waits for a task to complete, and returns it.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)
