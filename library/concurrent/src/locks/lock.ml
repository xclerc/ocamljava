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

open Class'java'util'concurrent'TimeUnit
open Class'java'util'concurrent'locks'Lock
open Class'java'util'concurrent'locks'ReentrantLock

type t = _'Lock java_instance

let make_reentrant ?(fair = false) () =
  Java.make "ReentrantLock(boolean)" fair
  |> Java.cast "Lock"

let lock l =
  Java.call "Lock.lock()" l

let lock_interruptibly l =
  Java.call "Lock.lockInterruptibly()" l

let new_condition l =
  Java.call "Lock.newCondition()" l

let try_lock l =
  Java.call "Lock.tryLock()" l

let try_lock_time l time timeunit =
  Java.call "Lock.tryLock(long,TimeUnit)" l time timeunit

let unlock l =
  Java.call "Lock.unlock()" l


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
