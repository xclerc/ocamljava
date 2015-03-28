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

type state =
  | New
  | Runnable
  | Blocked
  | Waiting
  | Timed_waiting
  | Terminated

let decode_state s =
  if Java.equal (Java.get "Thread.State.NEW" ()) s then
    New
  else if Java.equal (Java.get "Thread.State.RUNNABLE" ()) s then
    Runnable
  else if Java.equal (Java.get "Thread.State.BLOCKED" ()) s then
    Blocked
  else if Java.equal (Java.get "Thread.State.WAITING" ()) s then
    Waiting
  else if Java.equal (Java.get "Thread.State.TIMED_WAITING" ()) s then
    Timed_waiting
  else if Java.equal (Java.get "Thread.State.TERMINATED" ()) s then
    Terminated
  else
    assert false

let max_priority  = Java.get "Thread.MAX_PRIORITY"  ()

let min_priority  = Java.get "Thread.MIN_PRIORITY"  ()

let norm_priority = Java.get "Thread.NORM_PRIORITY" ()

type t = _'Thread java_instance

external ocamljava_thread_make : ThreadGroup.t -> JavaString.t -> ('a -> unit) -> 'a -> t =
  "ocamljava_thread_make"

let make ?(group = Java.null) ?(name = Java.null) f x =
  ocamljava_thread_make group name f x
  
let current_thread () =
  Java.call "Thread.currentThread()" ()

let get_id th =
  Java.call "Thread.getId()" th

let get_name th =
  Java.call "Thread.getName()" th

let get_priority th =
  Java.call "Thread.getPriority()" th

let get_state th =
  Java.call "Thread.getState()" th
  |> decode_state

let get_thread_group th =
  Java.call "Thread.getThreadGroup()" th

let interrupt th =
  Java.call "Thread.interrupt()" th

let interrupted () =
  Java.call "Thread.interrupted()" ()

let is_alive th =
  Java.call "Thread.isAlive()" th

let is_daemon th =
  Java.call "Thread.isDaemon()" th

let is_interrupted th =
  Java.call "Thread.isInterrupted()" th

let join th =
  Java.call "Thread.join()" th

let join_time th millis =
  Java.call "Thread.join(long)" th millis

let join_time_nanos th millis nanos =
  Java.call "Thread.join(long,int)" th millis nanos

let set_daemon th b =
  Java.call "Thread.setDaemon(boolean)" th b

let set_name th n =
  Java.call "Thread.setName(String)" th n

let set_priority th prio =
  Java.call "Thread.setPriority(int)" th prio

let sleep millis =
  Java.call "Thread.sleep(long)" millis

let sleep_nanos millis nanos =
  Java.call "Thread.sleep(long,int)" millis nanos

let start th =
  Java.call "Thread.start()" th

let yield () =
  Java.call "Thread.yield()" ()
