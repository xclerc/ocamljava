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

(** Reentrant locks. *)


type t
(** The type of (reentrant) locks. *)

external make_reentrant : bool -> t =
  "ocamljava_lock_make_reentrant"
(** Returns a new reentrant lock, the parameter indicates whether a
    {i fair} ordering policy is requested. *)

external lock : t -> unit =
  "ocamljava_lock_lock"
(** Acquires the lock. Returns immediately if the lock is either not held
    by another thread, or already held by the current thread. Otherwise,
    the current thread is blocked until the holding thread releases the
    lock. *)

external lock_interruptibly : t -> unit =
  "ocamljava_lock_lock_interruptibly"
(** Similar to [lock], except that some other thread may interrupt the
    current thread while blocked.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external new_condition : t -> Condition.t =
  "ocamljava_lock_new_condition"
(** Returns a new condition associated with the passed lock. *)

external try_lock : t -> bool =
  "ocamljava_lock_try_lock"
(** Acquires the lock if available, returning [true]. Otherwise,
    immediately returns [false]. *)

external try_lock_time : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_lock_try_lock_time"
(** [try_lock_time l t u] is similar to [lock_interruptibly l], except
    that the current thread will at most wait for [t] (time value whose
    unit is [u]). Returns whether the lock was acquired.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)


external unlock : t -> unit =
  "ocamljava_lock_unlock"
(** Releases the lock.

    Raises [Invalid_argument] if the current thread does not hold the
    lock. *)
