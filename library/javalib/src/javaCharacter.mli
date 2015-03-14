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

(** Utility functions for [char] type and [Character] class. *)


(** {6 Instance creation} *)

type t = java'lang'Character java_instance
(** The type of wrappers for character values. *)

val min_value : java_char
(** The smallest value for character values. *)
    
val max_value : java_char
(** The largest value for character values. *)

val make : java_char -> t
(** Returns a new {java java.lang.Character} wrapper for the passed value. *)


(** {6 Operations} *)

val char_value : t -> java_char
(** Returns the wrapped value. *)

val compare : java_char -> java_char -> java_int
(** Compares the passed values. *)

val compare_to : t -> t -> java_int
(** Compares the passed values. *)

val is_digit : java_char -> bool
(** Tests whether the passed value is a digit; see
    {java java.lang.Character#isDigit(char)}. *)

val is_letter : java_char -> bool
(** Tests whether the passed value is a letter; see
    {java java.lang.Character#isLetter(char)}. *)

val is_letter_or_digit : java_char -> bool
(** Tests whether the passed value is a letter or a digit; see
    {java java.lang.Character#isLetterOrDigit(char)}. *)

val is_lower_case : java_char -> bool
(** Tests whether the passed value is lower case; see
    {java java.lang.Character#isLowerCase(char)}. *)

val is_space_char : java_char -> bool
(** Tests whether the passed value is a space character; see
    {java java.lang.Character#isSpaceChar(char)}. *)

val is_upper_case : java_char -> bool
(** Tests whether the passed value is upper case; see
    {java java.lang.Character#isUpperCase(char)}. *)

val is_whitespace : java_char -> bool
(** Tests whether the passed value is a white space; see
    {java java.lang.Character#isWhitespace(char)}. *)

val to_lower_case : java_char -> java_char
(** Converts the passed value to lower case; see
    {java java.lang.Character#toLowerCase(char)}. *)

val to_upper_case : java_char -> java_char
(** Converts the passed value to upper case; see
    {java java.lang.Character#toUpperCase(char)}. *)

val to_string : java_char -> JavaString.t
(** Converts the passed character into a string; see
    {java java.lang.Character#toString(char)}. *)

val value_of : java_char -> t
(** Converts the passed bare character into a character wrapper;
    see {java java.lang.Character#valueOf(char)}. *)


(** {6 Conversion from/to OCaml characters} *)

external of_char : char -> java_char = "%identity"
(** Converts a plain OCaml [char] into a [java_char]. *)

val to_char : java_char -> char
(** Converts a [java_char] into a plain OCaml [char].

    @raise Invalid_argument if the passed [java_char] cannot be
           represented as a [char]. *)

(** {6 Null value} *)

val null : t
(** The [null] value. *)

external is_null : t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : t -> t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : t option -> t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)

(**/**)

external unsafe_to_char : java_char -> char = "%identity"
