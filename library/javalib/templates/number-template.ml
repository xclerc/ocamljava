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

type t = _'$(java_wrapper) java_instance

let min_value = Java.get "$(java_wrapper).MIN_VALUE" ()
    
let max_value = Java.get "$(java_wrapper).MAX_VALUE" ()

$(extra_constants_impl)

let make prim =
  Java.make "$(java_wrapper)($(java_primitive))" prim

let make_of_string str =
  Java.make "$(java_wrapper)(String)" str


(* Operations *)

let byte_value wrapper =
  Java.call "$(java_wrapper).byteValue()" wrapper

let double_value wrapper =
  Java.call "$(java_wrapper).doubleValue()" wrapper

let float_value wrapper =
  Java.call "$(java_wrapper).floatValue()" wrapper

let int_value wrapper =
  Java.call "$(java_wrapper).intValue()" wrapper

let long_value wrapper =
  Java.call "$(java_wrapper).longValue()" wrapper

let short_value wrapper =
  Java.call "$(java_wrapper).shortValue()" wrapper

let compare prim1 prim2 =
  Java.call "$(java_wrapper).compare($(java_primitive),$(java_primitive))" prim1 prim2

let compare_to wrapper1 wrapper2 =
  Java.call "$(java_wrapper).compareTo($(java_wrapper))" wrapper1 wrapper2

let to_string prim =
  Java.call "$(java_wrapper).toString($(java_primitive))" prim

let value_of prim =
  Java.call "$(java_wrapper).valueOf($(java_primitive))" prim

$(extra_operations_impl)


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
