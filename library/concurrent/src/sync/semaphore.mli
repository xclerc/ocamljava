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

(** Semaphores. *)


type t = java'util'concurrent'Semaphore java_instance
(** The type of semaphores, each maintaining a set of available
    permits. *)

val make : ?fair: bool -> java_int -> t
(** [make ~fair:f p] returns a new semaphore with [p] permits, [f]
    indicating whether a {i fair} ordering policy is requested
    (defaulting to [false]); see
    {java java.util.concurrent.Semaphore#Semaphore(int, boolean)}.

    [p] can be negative, meaning that permits should be released before
    any acquisition. *)

val acquire : t -> java_int -> unit
(** [acquire s p] acquires [p] permits from semaphore [s], blocking until
    they are available; see
    {java java.util.concurrent.Semaphore#acquire(int)}.

    @raise Java_exception if [p] is negative
    @raise Java_exception if the thread is interrupted *)

val acquire_uninterruptibly : t -> java_int -> unit
(** [acquire_uninterruptibly s p] is similar to [acquire s p], except
    that waiting thread cannot be interrupted; see
    {java java.util.concurrent.Semaphore#acquireUninterruptibly(int)}.

    @raise Java_exception if [p] is negative *)

val available_permits : t -> java_int
(** Returns the number of available permits for the semaphore; see
    {java java.util.concurrent.Semaphore#availablePermits()}. *)

val drain_permits : t -> java_int
(** Acquires and returns all available permits from the semaphore,
    returning immediately; see
    {java java.util.concurrent.Semaphore#drainPermits()}. *)

val get_queue_length : t -> java_int
(** Returns an estimate of the number of threads waiting on the semaphore
    to acquire permits; see
    {java java.util.concurrent.Semaphore#getQueueLength()}. *)

val has_queued_threads : t -> bool
(** Tests whether there are threads waiting on the semaphore to acquire
    permits; see
    {java java.util.concurrent.Semaphore#hasQueuedThreads()}. *)

val is_fair : t -> bool
(** Tests whether the semaphore uses a fair policy; see
    {java java.util.concurrent.Semaphore#isFair()}. *)

val release : t -> java_int -> unit
(** [release s p] releases [p] permits from semaphore [s]; see
    {java java.util.concurrent.Semaphore#release(int)}.

    @raise Java_exception if [p] is negative *)

val try_acquire : t -> java_int -> bool
(** [try_acquire s p] is similar to [acquire s p], except the function
    always returns immediately returning [true] if acquisition was
    successful; see
    {java java.util.concurrent.Semaphore#tryAcquire(int)}.

    @raise Java_exception if [p] is negative *)

val try_acquire_time : t -> java_int -> java_long -> TimeUnit.t -> bool
(** [try_acquire_time s p t u] is similar to [try_acquire s p], except
    that the current thread will at most wait for [t] (time value whose
    unit is [u]); see
    {java java.util.concurrent.Semaphore#tryAcquire(int, long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if [p] is negative
    @raise Java_exception if the thread is interrupted *)


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
