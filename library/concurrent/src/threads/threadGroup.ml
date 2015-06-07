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

type t = java'lang'ThreadGroup java_instance

let make ?(parent = Java.null) name =
  if Java.is_null parent then
    Java.make "ThreadGroup(String)" name
  else
    Java.make "ThreadGroup(ThreadGroup,String)" parent name

let active_count group =
  Java.call "ThreadGroup.activeCount()" group

let active_group_count group =
  Java.call "ThreadGroup.activeGroupCount()" group

let destroy group =
  Java.call "ThreadGroup.destroy()" group

let get_max_priority group =
  Java.call "ThreadGroup.getMaxPriority()" group

let get_name group =
  Java.call "ThreadGroup.getName()" group

let get_parent group =
  Java.call "ThreadGroup.getParent()" group

let interrupt group =
  Java.call "ThreadGroup.interrupt()" group

let is_daemon group =
  Java.call "ThreadGroup.isDaemon()" group

let is_destroyed group =
  Java.call "ThreadGroup.isDestroyed()" group

let parent_of group1 group2 =
  Java.call "ThreadGroup.parentOf(ThreadGroup)" group1 group2

let set_daemon group b =
  Java.call "ThreadGroup.setDaemon(boolean)" group b

let set_max_priority group prio =
  Java.call "ThreadGroup.setMaxPriority(int)" group prio

let enumerate_threads group ?(recurse = true) dest =
  Java.call "ThreadGroup.enumerate(Thread[],boolean)" group dest recurse

let enumerate_groups group ?(recurse = true) dest =
  Java.call "ThreadGroup.enumerate(ThreadGroup[],boolean)" group dest recurse


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
