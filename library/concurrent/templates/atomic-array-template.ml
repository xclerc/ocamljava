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

type index = int32

let make len =
  Java.make "$(java_class)(int)" len

let add_and_get atomic idx delta =
  Java.call "$(java_class).addAndGet(int,$(java_type))" atomic idx delta

let compare_and_set atomic idx expect update =
  Java.call "$(java_class).compareAndSet(int,$(java_type),$(java_type))" atomic idx expect update

let decrement_and_get atomic idx =
  Java.call "$(java_class).decrementAndGet(int)" atomic idx

let get atomic idx =
  Java.call "$(java_class).get(int)" atomic idx

let get_and_add atomic idx delta =
  Java.call "$(java_class).getAndAdd(int,$(java_type))" atomic idx delta

let get_and_decrement atomic idx =
  Java.call "$(java_class).getAndDecrement(int)" atomic idx

let get_and_increment atomic idx =
  Java.call "$(java_class).getAndIncrement(int)" atomic idx

let get_and_set atomic idx value =
  Java.call "$(java_class).getAndSet(int,$(java_type))" atomic idx value

let increment_and_get atomic idx =
  Java.call "$(java_class).incrementAndGet(int)" atomic idx

let lazy_set atomic idx value =
  Java.call "$(java_class).lazySet(int,$(java_type))" atomic idx value

let length atomic =
  Java.call "$(java_class).length()" atomic

let set atomic idx value =
  Java.call "$(java_class).set(int,$(java_type))" atomic idx value

let weak_compare_and_set atomic idx expect update =
  Java.call "$(java_class).weakCompareAndSet(int,$(java_type),$(java_type))" atomic idx expect update


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
