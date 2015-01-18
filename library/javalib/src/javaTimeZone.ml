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


(* Instance creation *)

type t = java'util'TimeZone java_instance

let make id =
  Java.call "java.util.TimeZone.getTimeZone(String)" id
    
let get_available_ids () =
  let arr = Java.call "java.util.TimeZone.getAvailableIDs()" () in
  let i = ref @@ JavaReferenceArray.length arr in
  let res = ref [] in
  while !i >= 0l do
    res := (JavaReferenceArray.get arr !i) :: !res
  done;
  !res

let get_default () =
  Java.call "java.util.TimeZone.getDefault()" ()

let set_default tz =
  Java.call "java.util.TimeZone.setDefault(java.util.TimeZone)" tz


(* Properties *)

let get_display_name tz =
  Java.call "java.util.TimeZone.getDisplayName()" tz

let get_id tz =
  Java.call "java.util.TimeZone.getID()" tz


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
