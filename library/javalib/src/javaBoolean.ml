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


(* Instance creation *)

type t = _'Boolean java_instance

let false_ = Java.get "Boolean.FALSE" ()

let true_ = Java.get "Boolean.TRUE" ()

let make bool =
  Java.make "Boolean(boolean)" bool

let make_of_string str =
  Java.make "Boolean(String)" str


(* Operations *)

let boolean_value bool =
  Java.call "Boolean.booleanValue()" bool

let compare bool1 bool2 =
  Java.call "Boolean.compare(boolean,boolean)" bool1 bool2

let compare_to bool1 bool2 =
  Java.call "Boolean.compareTo(Boolean)" bool1 bool2

let parse_boolean str =
  Java.call "Boolean.parseBoolean(String)" str

let to_string bool =
  Java.call "Boolean.toString(boolean)" bool

let value_of bool =
  Java.call "Boolean.valueOf(boolean)" bool

let value_of_string str =
  Java.call "Boolean.valueOf(String)" str


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
