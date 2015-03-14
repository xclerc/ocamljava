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


(* OCaml-compatible signature *)

module OCaml = struct

  let length s =
    Java.call "String.length()" s
    |> Int32.to_int

  let get s i =
    Java.call "String.charAt(int)" s (Int32.of_int i)

  let make n c =
    let res = Java.make "StringBuilder(int)" (Int32.of_int n) in
    for _i = 1 to n do
      Java.exec "StringBuilder.append(char):StringBuilder" res c
    done;
    Java.call "StringBuilder.toString()" res

  let copy s =
    s

  let sub s ofs len =
    Java.call "String.substring(int,int)"
      s
      (Int32.of_int ofs)
      (Int32.of_int (ofs + len))

  let concat sep l =
    let res = Java.make "StringBuilder()" () in
    begin match l with
    | hd :: tl ->
        Java.exec "StringBuilder.append(CharSequence):StringBuilder" res hd;
        List.iter
          (fun s ->
            Java.exec "StringBuilder.append(CharSequence):StringBuilder" res sep;
            Java.exec "StringBuilder.append(CharSequence):StringBuilder" res s)
          tl
    | [] -> ()
    end;
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
      Java.exec "StringBuilder.append(char):StringBuilder" res (f (get s i))
    done;
    Java.call "StringBuilder.toString()" res

  let trim s =
    Java.call "String.trim()" s

  let escaped s =
    let len = Java.call "String.length()" s in
    let res = Java.make "StringBuilder(int)" len in
    for i = 0 to pred (Int32.to_int len) do
      let ch = get s i in
      match ch with
      | 34 (* double quote *) | 92 (* back slash *) ->
          Java.exec "StringBuilder.append(char):StringBuilder" res 92;
          Java.exec "StringBuilder.append(char):StringBuilder" res ch
      | 10 (* new line *) ->
          Java.exec "StringBuilder.append(String):StringBuilder" res !@"\\n"
      | 9 (* tab *) ->
          Java.exec "StringBuilder.append(String):StringBuilder" res !@"\\t"
      | 13 (* carriage return *) ->
          Java.exec "StringBuilder.append(String):StringBuilder" res !@"\\r"
      | 8 (* backspace *) ->
          Java.exec "StringBuilder.append(String):StringBuilder" res !@"\\b"
      | _ ->
          Java.exec "StringBuilder.append(char):StringBuilder" res ch
    done;
    Java.call "StringBuilder.toString()" res

  let index s c =
    Java.call "String.indexOf(int)" s (Int32.of_int c)
    |> Int32.to_int

  let rindex s c =
    Java.call "String.lastIndexOf(int)" s (Int32.of_int c)
    |> Int32.to_int

  let index_from s i c =
    Java.call "String.indexOf(int,int)" s (Int32.of_int c) (Int32.of_int i)
    |> Int32.to_int

  let rindex_from s i c =
    Java.call "String.lastIndexOf(int,int)" s (Int32.of_int c) (Int32.of_int i)
    |> Int32.to_int

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
    Java.exec "StringBuilder.append(char):StringBuilder" res first;
    for i = 1 to pred (Int32.to_int len) do
      Java.exec "StringBuilder.append(char):StringBuilder" res (get s i)
    done;
    Java.call "StringBuilder.toString()" res

  let uncapitalize s =
    let len = Java.call "String.length()" s in
    let res = Java.make "StringBuilder(int)" len in
    let first = Java.call "Character.toLowerCase(char)" (get s 0) in
    Java.exec "StringBuilder.append(char):StringBuilder" res first;
    for i = 1 to pred (Int32.to_int len) do
      Java.exec "StringBuilder.append(char):StringBuilder" res (get s i)
    done;
    Java.call "StringBuilder.toString()" res

  type t = _'String java_instance

  let compare x y =
    Java.call "String.compareTo(String)" x y
    |> Int32.to_int

  let compare_ignore_case x y =
    Java.call "String.compareToIgnoreCase(String)" x y
    |> Int32.to_int

end


(* String operations *)

type t = _'String java_instance

let equals x y =
  Java.call "String.equals(Object)" x y

let equals_ignore_case x y =
  Java.call "String.equalsIgnoreCase(String)" x y


(* Conversion from/to OCaml strings *)

external of_string : string -> _'String java_instance =
  "ocamljava_javastring_of_string"

external to_string : _'String java_instance -> string =
  "ocamljava_javastring_to_string"


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : _'String java_instance -> bool =
  "java is_null"

external is_not_null : _'String java_instance -> bool =
  "java is_not_null"


(* Output functions *)

external print_string : _'String java_instance -> unit =
  "ocamljava_javastring_print_string"

external print_endline : _'String java_instance -> unit =
  "ocamljava_javastring_print_endline"

external prerr_string : _'String java_instance -> unit =
  "ocamljava_javastring_prerr_string"

external prerr_endline : _'String java_instance -> unit =
  "ocamljava_javastring_prerr_endline"

external output_string : out_channel -> _'String java_instance -> unit =
  "ocamljava_javastring_output_string"


(* Input functions *)

external read_line : unit -> _'String java_instance =
  "ocamljava_javastring_read_line"

external input_line : in_channel -> _'String java_instance =
  "ocamljava_javastring_input_line"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
