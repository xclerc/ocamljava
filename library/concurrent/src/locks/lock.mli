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


type t = java'util'concurrent'locks'Lock java_instance
(** The type of (reentrant) locks. *)

val make_reentrant : ?fair:bool -> unit -> t
(** Returns a new reentrant lock, the parameter indicates whether a
    {i fair} ordering policy is requested (defaulting to [false]); see
    {java java.util.concurrent.locks.ReentrantLock#ReentrantLock(boolean)}. *)

val lock : t -> unit
(** Acquires the lock. Returns immediately if the lock is either not held
    by another thread, or already held by the current thread. Otherwise,
    the current thread is blocked until the holding thread releases the
    lock; see
    {java java.util.concurrent.locks.Lock#lock()}. *)

val lock_interruptibly : t -> unit
(** Similar to {!lock}, except that some other thread may interrupt the
    current thread while blocked; see
    {java java.util.concurrent.locks.Lock#lockInterruptibly()}.

    @raise Java_exception if the thread is interrupted *)

val new_condition : t -> Condition.t
(** Returns a new condition associated with the passed lock; see
    {java java.util.concurrent.locks.Lock#newCondition()}. *)

val try_lock : t -> bool
(** Acquires the lock if available, returning [true]. Otherwise,
    immediately returns [false]; see
    {java java.util.concurrent.locks.Lock#tryLock()}. *)

val try_lock_time : t -> java_long -> TimeUnit.t -> bool
(** [try_lock_time l t u] is similar to [lock_interruptibly l], except
    that the current thread will at most wait for [t] (time value whose
    unit is [u]). Returns whether the lock was acquired; see
    {java java.util.concurrent.locks.Lock#tryLock(long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted *)

val unlock : t -> unit
(** Releases the lock; see
    {java java.util.concurrent.locks.Lock#unlock()}.

    @raise Java_exception if the current thread does not hold the lock *)
