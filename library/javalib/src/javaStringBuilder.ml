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

type t = _'StringBuilder java_instance

let make () =
  Java.make "StringBuilder()" ()

let make_of_capacity cap =
  Java.make "StringBuilder(int)" cap

let make_of_char_sequence seq =
  Java.make "StringBuilder(CharSequence)" seq

let make_of_string str =
  Java.make "StringBuilder(String)" str


(* Capacity and length *)

let capacity sb =
  Java.call "StringBuilder.capacity()" sb

let ensure_capacity sb cap =
  Java.call "StringBuilder.ensureCapacity(int)" sb cap

let length sb =
  Java.call "StringBuilder.length()" sb

let set_length sb len =
  Java.call "StringBuilder.setLength(int)" sb len

let trim_to_size sb =
  Java.call "StringBuilder.trimToSize()" sb


(* Value addition *)

let append_boolean sb value =
  Java.call "StringBuilder.append(boolean):StringBuilder" sb value

let append_char sb value =
  Java.call "StringBuilder.append(char):StringBuilder" sb value

let append_double sb value =
  Java.call "StringBuilder.append(double):StringBuilder" sb value

let append_float sb value =
  Java.call "StringBuilder.append(float):StringBuilder" sb value

let append_int sb value =
  Java.call "StringBuilder.append(int):StringBuilder" sb value

let append_long sb value =
  Java.call "StringBuilder.append(long):StringBuilder" sb value

let append_string sb value =
  Java.call "StringBuilder.append(String):StringBuilder" sb value

let append_code_point sb value =
  Java.call "StringBuilder.appendCodePoint(int):StringBuilder" sb value


(* Value insertion *)

let insert_boolean sb idx value =
  Java.call "StringBuilder.insert(int,boolean):StringBuilder" sb idx value

let insert_char sb idx value =
  Java.call "StringBuilder.insert(int,char):StringBuilder" sb idx value

let insert_double sb idx value =
  Java.call "StringBuilder.insert(int,double):StringBuilder" sb idx value

let insert_float sb idx value =
  Java.call "StringBuilder.insert(int,float):StringBuilder" sb idx value

let insert_int sb idx value =
  Java.call "StringBuilder.insert(int,int):StringBuilder" sb idx value

let insert_long sb idx value =
  Java.call "StringBuilder.insert(int,long):StringBuilder" sb idx value

let insert_string sb idx value =
  Java.call "StringBuilder.insert(int,String):StringBuilder" sb idx value


(* Character lookup and modification *)

let char_at sb idx =
  Java.call "StringBuilder.charAt(int)" sb idx

let set_char_at sb idx value =
  Java.call "StringBuilder.setCharAt(int,char)" sb idx value

let code_point_at sb idx =
  Java.call "StringBuilder.codePointAt(int)" sb idx


(* Deletion *)

let delete sb start end_ =
  Java.call "StringBuilder.delete(int,int):StringBuilder" sb start end_

let delete_char_at sb idx =
  Java.call "StringBuilder.deleteCharAt(int):StringBuilder" sb idx


(* Replacement *)

let replace sb start end_ =
  Java.call "StringBuilder.replace(int,int,String):StringBuilder" sb start end_


(* Search *)

let index_of sb str =
  Java.call "StringBuilder.indexOf(String)" sb str

let index_of_from sb str idx =
  Java.call "StringBuilder.indexOf(String,int)" sb str idx

let last_index_of sb str =
  Java.call "StringBuilder.lastIndexOf(String)" sb str

let last_index_of_from sb str idx =
  Java.call "StringBuilder.indexOf(String,int)" sb str idx


(* Conversion *)

let substring sb start end_ =
  Java.call "StringBuilder.substring(int,int)" sb start end_

let to_string sb =
  Java.call "StringBuilder.toString()" sb


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
