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

type t = _'Character java_instance

let min_value = Java.get "Character.MIN_VALUE" ()
    
let max_value = Java.get "Character.MAX_VALUE" ()

let make char =
  Java.make "Character(char)" char


(* Operations *)

let char_value char =
  Java.call "Character.charValue()" char

let compare char1 char2 =
  Java.call "Character.compare(char,char)" char1 char2

let compare_to char1 char2 =
  Java.call "Character.compareTo(Character)" char1 char2

let is_digit char =
  Java.call "Character.isDigit(char)" char

let is_letter char =
  Java.call "Character.isLetter(char)" char

let is_letter_or_digit char =
  Java.call "Character.isLetterOrDigit(char)" char

let is_lower_case char =
  Java.call "Character.isLowerCase(char)" char

let is_space_char char =
  Java.call "Character.isSpaceChar(char)" char

let is_upper_case char =
  Java.call "Character.isUpperCase(char)" char

let is_whitespace char =
  Java.call "Character.isWhitespace(char)" char

let to_lower_case char =
  Java.call "Character.toLowerCase(char)" char

let to_upper_case char =
  Java.call "Character.toUpperCase(char)" char

let to_string char =
  Java.call "Character.toString(char)" char

let value_of char =
  Java.call "Character.valueOf(char)" char


(* Conversion from/to OCaml characters *)

external of_char : char -> java_char = "%identity"

external unsafe_to_char : java_char -> char = "%identity"

let to_char ch =
  if (ch >= 0) && (ch <= 255) then
    unsafe_to_char ch
  else
    invalid_arg "JavaCharacter.to_char"

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
