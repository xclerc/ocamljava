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

type t = $(ocaml_java_type)

let make value =
  Java.make "$(java_class)($(java_type))" value

$(add_and_get)

let compare_and_set atomic expect update =
  Java.call "$(java_class).compareAndSet($(java_type),$(java_type))"
    atomic expect update

$(decrement_and_get)

let get atomic =
  Java.call "$(java_class).get()" atomic

$(get_and_add)

$(get_and_decrement)

$(get_and_increment)

let get_and_set atomic value =
  Java.call "$(java_class).getAndSet($(java_type))" atomic value

$(increment_and_get)

let lazy_set atomic value =
  Java.call "$(java_class).lazySet($(java_type))" atomic value

let set atomic value =
  Java.call "$(java_class).set($(java_type))" atomic value

let weak_compare_and_set atomic expect update =
  Java.call "$(java_class).weakCompareAndSet($(java_type),$(java_type))"
    atomic expect update


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
