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

(** Semaphores. *)

type t
(** The type of semaphores, each maintaining a set of available
    permits. *)

external make : int32 -> bool -> t =
  "ocamljava_semaphore_make"
(** [make p f] returns a new semaphore with [p] permits, [f] indicating
    whether a {i fair} ordering policy is requested.

    [p] can be negative, meaning that permits should be released before
    any acquisition. *)

external acquire : t -> int32 -> unit =
  "ocamljava_semaphore_acquire"
(** [acquire s p] acquires [p] permits from semaphore [s], blocking until
    they are available.

    Raises [Invalid_argument] if [p] is negative.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external acquire_uninterruptibly : t -> int32 -> unit =
  "ocamljava_semaphore_acquire_uninterruptibly"
(** [acquire_uninterruptibly s p] is similar to [acquire s p], except
    that waiting thread cannot be interrupted.

    Raises [Invalid_argument] if [p] is negative. *)

external available_permits : t -> int32 =
  "ocamljava_semaphore_available_permits"
(** Returns the number of available permits for the semaphore. *)

external drain_permits : t -> int32 =
  "ocamljava_semaphore_drain_permits"
(** Acquires and returns all available permits from the semaphore,
    returning immediately. *)

external get_queue_length : t -> int32 =
  "ocamljava_semaphore_get_queue_length"
(** Returns an estimate of the number of threads waiting on the semaphore
    to acquire permits. *)

external has_queued_threads : t -> bool =
  "ocamljava_semaphore_has_queued_threads"
(** Tests whether there are threads waiting on the semaphore to acquire
    permits. *)

external is_fair : t -> bool =
  "ocamljava_semaphore_is_fair"
(** Tests whether the semaphore uses a fair policy. *)

external release : t -> int32 -> unit =
  "ocamljava_semaphore_release"
(** [release s p] releases [p] permits from semaphore [s].

    Raises [Invalid_argument] if [p] is negative. *)

external try_acquire : t -> int32 -> bool =
  "ocamljava_semaphore_try_acquire"
(** [try_acquire s p] is similar to [acquire s p], except the function
    always returns immediately returning [true] if acquisition was
    successful.

    Raises [Invalid_argument] if [p] is negative. *)

external try_acquire_time : t -> int32 -> int64 -> TimeUnit.t -> bool =
  "ocamljava_semaphore_try_acquire"
(** [try_acquire_time s p t u] is similar to [try_acquire s p], except
    that the current thread will at most wait for [t] (time value whose
    unit is [u]).

    Raises [Invalid_argument] if [p] is negative.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)
