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


type 'a t = 'a java_reference_array


(* Usual operations *)

external length : 'a java_reference_array -> int32 =
  "java array length reference"

external get : 'a java_reference_array -> int32 -> 'a =
  "java array get reference"

external set : 'a java_reference_array -> int32 -> 'a -> unit =
  "java array set reference"

external to_object : 'a java_reference_array -> java'lang'Object java_instance =
  "java array to_object reference"

let blit src srcofs dst dstofs len =
  Java.call "System.arraycopy(Object,int,Object,int,int)"
    (to_object src) srcofs (to_object dst) dstofs len

let iter f a =
  let len = length a in
  let i = ref 0l in
  while !i < len do
    f (get a !i);
    i := Int32.succ !i
  done

let iteri f a =
  let len = length a in
  let i = ref 0l in
  while !i < len do
    f !i (get a !i);
    i := Int32.succ !i
  done

let fold_left f z a =
  let res = ref z in
  let len = length a in
  let i = ref 0l in
  while !i < len do
    res := f !res (get a !i);
    i := Int32.succ !i
  done;
  !res

let fold_right f a z =
  let res = ref z in
  let i = ref (Int32.pred (length a)) in
  while !i >= 0l do
    res := f (get a !i) !res;
    i := Int32.pred !i
  done;
  !res


(* Java operations *)

external of_object : java'lang'Object java_instance -> 'a java_reference_array =
  "java array of_object reference"

external null : unit -> 'a java_reference_array =
  "java null"

let null = Obj.magic @@ null ()

external is_null : 'a java_reference_array -> bool =
  "java is_null"

external is_not_null : 'a java_reference_array -> bool =
  "java is_not_null"

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
