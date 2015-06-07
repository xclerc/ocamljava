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

open Class'java'util'concurrent'Semaphore
open Class'java'util'concurrent'TimeUnit

type t = _'Semaphore java_instance

let make ?(fair = false) permits =
  Java.make "Semaphore(int,boolean)" permits fair

let acquire sem permits =
  Java.call "Semaphore.acquire(int)" sem permits

let acquire_uninterruptibly sem permits =
  Java.call "Semaphore.acquireUninterruptibly(int)" sem permits

let available_permits sem =
  Java.call "Semaphore.availablePermits()" sem

let drain_permits sem =
  Java.call "Semaphore.drainPermits()" sem

let get_queue_length sem =
  Java.call "Semaphore.getQueueLength()" sem

let has_queued_threads sem =
  Java.call "Semaphore.hasQueuedThreads()" sem

let is_fair sem =
  Java.call "Semaphore.isFair()" sem

let release sem permits =
  Java.call "Semaphore.release(int)" sem permits

let try_acquire sem permits =
  Java.call "Semaphore.tryAcquire(int)" sem permits

let try_acquire_time sem permits time timeunit =
  Java.call "Semaphore.tryAcquire(int,long,TimeUnit)"sem permits time timeunit


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
