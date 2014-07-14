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


(* String operations *)

let length s =
  Int32.to_int (Java.call "String.length()" s)

let get s i =
  Java.call "String.charAt(_)" s (Int32.of_int i)

let make n c =
  let res = Java.make "StringBuilder(int)" (Int32.of_int n) in
  for _i = 1 to n do
    Java.call "StringBuilder.append(char):StringBuilder" res c
    |> ignore 
  done;
  Java.call "StringBuilder.toString()" res

let copy s =
  s

let sub s ofs len =
  Java.call "String.substring(_,_)" s (Int32.of_int ofs) (Int32.of_int (ofs + len))

let concat sep l =
  let res = Java.make "StringBuilder()" () in
  (match l with
  | hd :: tl ->
      ignore (Java.call "StringBuilder.append(CharSequence):StringBuilder" res hd);
      List.iter
        (fun s ->
          Java.call "StringBuilder.append(CharSequence):StringBuilder" res sep
          |> ignore;
          Java.call "StringBuilder.append(CharSequence):StringBuilder" res s
          |> ignore)
        tl
  | [] -> ());
  Java.call "StringBuilder.toString()" res

let iter f s =
  let len = length s in
  for i = 0 to pred len do
    f (get s i)
  done

let iteri f s =
  let len = length s in
  for i = 0 to pred len do
    f i (get s i)
  done

let map f s =
  let len = Java.call "String.length()" s in
  let res = Java.make "StringBuilder(int)" len in
  for i = 0 to pred (Int32.to_int len) do
    Java.call "StringBuilder.append(char):StringBuilder" res (f (get s i))
    |> ignore
  done;
  Java.call "StringBuilder.toString()" res

let trim s =
  Java.call "String.trim()" s

external js_of_s : string -> _'String java_instance =
  "ocamljava_javastring_of_string"

let new_line = js_of_s "\\n"
let tab = js_of_s "\\t"
let carriage_return = js_of_s "\\r"
let backspace = js_of_s "\\b"

let escaped s =
  let len = Java.call "String.length()" s in
  let res = Java.make "StringBuilder(int)" len in
  for i = 0 to pred (Int32.to_int len) do
    let ch = get s i in
    (match ch with
    | 34 (* double quote *) | 92 (* back slash *) ->
        ignore (Java.call "StringBuilder.append(char):StringBuilder" res 92);
        ignore (Java.call "StringBuilder.append(char):StringBuilder" res ch)
    | 10 (* new line *) ->
        ignore (Java.call "StringBuilder.append(String):StringBuilder" res new_line)
    | 9 (* tab *) ->
        ignore (Java.call "StringBuilder.append(String):StringBuilder" res tab)
    | 13 (* carriage return *) ->
        ignore (Java.call "StringBuilder.append(String):StringBuilder" res carriage_return)
    | 8 (* backspace *) ->
        ignore (Java.call "StringBuilder.append(String):StringBuilder" res backspace)
    | _ ->
        ignore (Java.call "StringBuilder.append(char):StringBuilder" res ch))
  done;
  Java.call "StringBuilder.toString()" res

let index s c =
  Int32.to_int (Java.call "String.indexOf(int)" s (Int32.of_int c))

let rindex s c =
  Int32.to_int (Java.call "String.lastIndexOf(int)" s (Int32.of_int c))

let index_from s i c =
  Int32.to_int (Java.call "String.indexOf(int,int)" s (Int32.of_int c) (Int32.of_int i))

let rindex_from s i c =
  Int32.to_int (Java.call "String.lastIndexOf(int,int)" s (Int32.of_int c) (Int32.of_int i))

let contains_from s i c =
  let len = length s in
  let i = ref i in
  while (!i < len) && ((get s !i) <> c) do
    incr i
  done;
  !i < len

let contains s c =
  contains_from s 0 c

let rcontains_from s i c =
  let i = ref i in
  while (!i >= 0) && ((get s !i) <> c) do
    decr i
  done;
  !i >= 0

let uppercase s =
  Java.call "String.toUpperCase()" s

let lowercase s =
  Java.call "String.toLowerCase()" s

let capitalize s =
  let len = Java.call "String.length()" s in
  let res = Java.make "StringBuilder(int)" len in
  let first = Java.call "Character.toUpperCase(char)" (get s 0) in
  Java.call "StringBuilder.append(char):StringBuilder" res first
  |> ignore;
  for i = 1 to pred (Int32.to_int len) do
    Java.call "StringBuilder.append(char):StringBuilder" res (get s i)
    |> ignore
  done;
  Java.call "StringBuilder.toString()" res

let uncapitalize s =
  let len = Java.call "String.length()" s in
  let res = Java.make "StringBuilder(int)" len in
  let first = Java.call "Character.toLowerCase(char)" (get s 0) in
  Java.call "StringBuilder.append(char):StringBuilder" res first
  |> ignore;
  for i = 1 to pred (Int32.to_int len) do
    Java.call "StringBuilder.append(char):StringBuilder" res (get s i)
    |> ignore
  done;
  Java.call "StringBuilder.toString()" res

type t = java'lang'String java_instance

let compare x y =
  Int32.to_int (Java.call "String.compareTo(String)" x y)

let compare_ignore_case x y =
  Int32.to_int (Java.call "String.compareToIgnoreCase(_)" x y)


(* Conversion from/to OCaml strings *)

external of_string : string -> _'String java_instance =
  "ocamljava_javastring_of_string"

external to_string : _'String java_instance -> string =
  "ocamljava_javastring_to_string"


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : java'lang'String java_instance -> bool =
  "java is_null"

external is_not_null : java'lang'String java_instance -> bool =
  "java is_not_null"


(* Output functions *)

external print_string : java'lang'String java_instance -> unit =
  "ocamljava_javastring_print_string"

external print_endline : java'lang'String java_instance -> unit =
  "ocamljava_javastring_print_endline"

external prerr_string : java'lang'String java_instance -> unit =
  "ocamljava_javastring_prerr_string"

external prerr_endline : java'lang'String java_instance -> unit =
  "ocamljava_javastring_prerr_endline"

external output_string : out_channel -> java'lang'String java_instance -> unit =
  "ocamljava_javastring_output_string"


(* Input functions *)

external read_line : unit -> java'lang'String java_instance =
  "ocamljava_javastring_read_line"

external input_line : in_channel -> java'lang'String java_instance =
  "ocamljava_javastring_input_line"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x
