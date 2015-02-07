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

(** Computations run in background. *)


type 'a t
(** The type of futures, that are computations run in background. *)

external cancel : 'a t -> bool -> bool =
  "ocamljava_future_cancel"
(** [cancel f i] attemps to cancel future [f], [i] indicating whether to
    interrupt the computation if already started. Returns whether the
    future was cancelled. *)

external get : 'a t -> 'a =
  "ocamljava_future_get"
(** Waits for the computation to complete, and returns its result.

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Runtime.Raised] is the computation raised an uncaught
    exception.

    Raises [Not_found] if the computation was cancelled. *)

external get_time : 'a t -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_future_get_time"
(** [get_time f t u] is similar to [get f], except that the current
    thread will at most wait for [t] (time value whose unit is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Runtime.Raised] is the computation raised an uncaught
    exception.

    Raises [Not_found] if the computation was cancelled.

    Raises [Runtime.Timeout] if time has elapsed without completion. *)

external is_cancelled : 'a t -> bool =
  "ocamljava_future_is_cancelled"
(** Tests whether the task was cancelled before completion. *)

external is_done : 'a t -> bool =
  "ocamljava_future_is_done"
(** Tests whether the computation is completed. *)
