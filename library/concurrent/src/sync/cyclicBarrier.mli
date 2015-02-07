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

(** Reusable barriers. *)


type t
(** The type of cyclic barriers, that are barriers possibly re-used once
    all waiting threads have restarted. *)

external make : int32 -> t =
  "ocamljava_cyclicbarrier_make"
(** [make n] returns a barrier waiting for [n] threads.

    Raises [Invalid_argument] if [n] is negative. *)

external await : t -> int32 =
  "ocamljava_cyclicbarrier_await"
(** Waits until all threads have reached the barrier.

    Raises [Invalid_argument] if the barrier is broken.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external await_time : t -> int64 -> TimeUnit.t -> int32 =
  "ocamljava_cyclicbarrier_await"
(** [await_time b t u] is similar to [await b], except that the current
    thread will at most wait for [t] (time value whose unit is [u]).

    Raises [Invalid_argument] if the barrier is broken.

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Runtime.Timeout] if time has elapsed without gathering all
    threads. *)

external get_number_waiting : t -> int32 =
  "ocamljava_cyclicbarrier_get_number_waiting"
(** Returns the number of threads currently waiting on the barrier. *)

external get_parties : t -> int32 =
  "ocamljava_cyclicbarrier_get_parties"
(** Returns the number of threads to be waited on the barrier. *)

external is_broken : t -> bool =
  "ocamljava_cyclicbarrier_is_broken"
(** Tests whether the barrier is broken. A barrier is broken if [reset]
    when threads are waiting on it. *)

external reset : t -> unit =
  "ocamljava_cyclicbarrier_reset"
(** Resets the barrier to its original state. The barrier will be broken
    if there are threads currently waiting on it. *)
