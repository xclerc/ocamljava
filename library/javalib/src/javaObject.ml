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

type t = java'lang'Object java_instance

let make () =
  Java.make "Object()" ()


(* Base methods *)

let equals this that =
  Java.call "Object.equals(Object)" this that

let get_class x =
  Java.call "Object.getClass()" x

let hash_code x =
  Java.call "Object.hashCode()" x

let to_string x =
  Java.call "Object.toString()" x


(* Synchronization *)

let notify x =
  Java.call "Object.notify()" x

let notify_all x =
  Java.call "Object.notifyAll()" x

let wait x =
  Java.call "Object.wait()" x

let wait_timeout x ms =
  Java.call "Object.wait(long)" x ms

let wait_timeout_nanos x ms ns =
  Java.call "Object.wait(long,int)" x ms ns


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"


(* Miscellaneous *)

external cast : java'lang'Object java_extends -> t =
  "%identity"

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
