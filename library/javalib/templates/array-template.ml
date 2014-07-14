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


type e = $(ocaml_element_type)

type 'a t = 'a $(ocaml_java_type)


(* Usual operations *)

let make len =
  Java.make_array "$(java_element_type)[]" len

external length : e $(ocaml_java_type) -> int32 =
  "java array length $(java_element_type)"

external get : e $(ocaml_java_type) -> int32 -> e =
  "java array get $(java_element_type)"

external set : e $(ocaml_java_type) -> int32 -> e -> unit =
  "java array set $(java_element_type)"

let init len f =
  let res = Java.make_array "$(java_element_type)[]" len in
  let i = ref 0l in
  while !i < len do
    set res !i (f !i);
    i := Int32.succ !i
  done;
  res

external to_object : e $(ocaml_java_type) -> java'lang'Object java_instance =
  "java array to_object $(java_element_type)"

let append a1 a2 =
  let len1 = length a1 in
  let len2 = length a2 in
  let res = Java.make_array "$(java_element_type)[]" (Int32.add len1 len2) in
  Java.call "System.arraycopy(-)" (to_object a1) 0l (to_object res) 0l len1;
  Java.call "System.arraycopy(-)" (to_object a2) 0l (to_object res) len1 len2;
  res

let concat l =
  let total_len = List.fold_left (fun acc elem -> Int32.add acc (length elem)) 0l l in
  let res = Java.make_array "$(java_element_type)[]" total_len in
  let ofs = ref 0l in
  List.iter
    (fun a ->
      let len = length a in
      Java.call "System.arraycopy(-)" (to_object a) 0l (to_object res) !ofs len;
      ofs := Int32.add !ofs len)
    l;
  res

let sub a ofs len =
  Java.call "java.util.Arrays.copyOfRange($(java_element_type)[],_,_)"
    a
    ofs
    (Int32.add ofs len)

let copy a =
  Java.call "java.util.Arrays.copyOf($(java_element_type)[],_)"
    a
    (length a)

let fill a ofs len x =
  Java.call "java.util.Arrays.fill($(java_element_type)[],_,_,_)"
    a
    ofs
    (Int32.add ofs len)
    x

let blit src srcofs dst dstofs len =
  Java.call "System.arraycopy(-)" (to_object src) srcofs (to_object dst) dstofs len

let to_list a =
  let rec tl acc i =
    if i < 0l then
      acc
    else
      tl ((get a i) :: acc) (Int32.pred i) in
  tl [] (Int32.pred (length a))

let of_list l =
  let len = List.length l in
  if len > (Int32.to_int Int32.max_int) then
    invalid_arg "$(module_name).of_list"
  else begin
    let res = Java.make_array "$(java_element_type)[]" (Int32.of_int len) in
    let rec visit a i = function
      | hd :: tl ->
          set a i hd;
          visit a (Int32.succ i) tl
      | [] ->
          () in
    visit res 0l l;
    res
  end

let iter f a =
  let len = length a in
  let i = ref 0l in
  while !i < len do
    f (get a !i);
    i := Int32.succ !i
  done

let map f a =
  let len = length a in
  let res = Java.make_array "$(java_element_type)[]" len in
  let i = ref 0l in
  while !i < len do
    set res !i (f (get a !i));
    i := Int32.succ !i
  done;
  res

let iteri f a =
  let len = length a in
  let i = ref 0l in
  while !i < len do
    f !i (get a !i);
    i := Int32.succ !i
  done

let mapi f a =
  let len = length a in
  let res = Java.make_array "$(java_element_type)[]" len in
  let i = ref 0l in
  while !i < len do
    set res !i (f !i (get a !i));
    i := Int32.succ !i
  done;
  res

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


(* Conversion from/to OCaml arrays *)

let of_ocaml a =
  let len = Array.length a in
  if len > (Int32.to_int Int32.max_int) then
    invalid_arg "$(module_name).of_ocaml"
  else begin
    let len = Int32.of_int len in
    let res = Java.make_array "$(java_element_type)[]" len in
    let i = ref 0l in
    while !i < len do
      set res !i a.(Int32.to_int !i);
      i := Int32.succ !i
    done;
    res
  end

let to_ocaml a =
  let len = length a in
  let res = Array.make (Int32.to_int len) $(zero_value) in
  let i = ref 0l in
  while !i < len do
    res.(Int32.to_int !i) <- get a !i;
    i := Int32.succ !i
  done;
  res


(* Java operations *)

external of_object : java'lang'Object java_instance -> e $(ocaml_java_type) =
  "java array of_object $(java_element_type)"

external null : unit -> e $(ocaml_java_type) =
  "java null"

let null = null ()

external is_null : e $(ocaml_java_type) -> bool =
  "java is_null"

external is_not_null : e $(ocaml_java_type) -> bool =
  "java is_not_null"

let wrap x =
  if is_null x then
    None
  else
    Some x
$(extra_impl)
