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

external make : 'a java_constructor -> 'a =
  "java constructor"

external make_array : 'a java_array_shape -> 'a =
  "java make array"

external make_array_dims : 'a java_array_shape_dims -> 'a =
  "java make array dims"


(* Method call *)

external call : 'a java_method -> 'a =
  "java method call"


(* Field access *)

external get : 'a java_field_get -> 'a =
  "java field get"

external set : 'a java_field_set -> 'a =
  "java field set"


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"


(* Equality test *)

external equal : 'a java_instance -> 'b java_instance -> bool =
  "java =="

external not_equal : 'a java_instance -> 'b java_instance -> bool =
  "java !="


(* Class test *)

external instanceof : 'a java_reference_type -> 'b java_instance -> bool =
  "java instanceof"

external cast : 'a java_reference_type -> 'b java_instance -> 'a =
  "java cast"


(* Class retrieval *)

external get_class : 'a java_any_type -> java'lang'Class java_instance =
  "java class"


(* Exception throw *)

external throw : java'lang'Throwable java_extends -> 'a =
  "java throw"


(* Synchronization *)

external synchronized : 'a java_instance -> (unit -> unit) -> unit =
  "java synchronized"


(* Interface implementation *)

external proxy : 'a java_proxy -> 'a =
  "java proxy"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x
