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

(** Utility functions for Java strings. *)


(** {6 OCaml-compatible signature} *)

module OCaml : sig

  val length : java'lang'String java_instance -> int
  (** Similar to {!String.length}. *)

  val get : java'lang'String java_instance -> int -> int
  (** Similar to {!String.get}. *)

  val make : int -> int -> java'lang'String java_instance
  (** Similar to {!String.make}. *)

  val copy : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.copy}, equivalent to the identity function. *)

  val sub : java'lang'String java_instance -> int -> int -> java'lang'String java_instance
  (** Similar to {!String.sub}. *)

  val concat : java'lang'String java_instance -> java'lang'String java_instance list -> java'lang'String java_instance
  (** Similar to {!String.concat}. *)

  val iter : (int -> unit) -> java'lang'String java_instance -> unit
  (** Similar to {!String.iter}. *)

  val iteri : (int -> int -> unit) -> java'lang'String java_instance -> unit
  (** Similar to {!String.iteri}. *)

  val map : (int -> int) -> java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.map}. *)

  val trim : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.trim}. *)

  val escaped : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.escaped}. *)

  val index : java'lang'String java_instance -> int -> int
  (** Similar to {!String.index}. *)

  val rindex : java'lang'String java_instance -> int -> int
  (** Similar to {!String.rindex}. *)

  val index_from : java'lang'String java_instance -> int -> int -> int
  (** Similar to {!String.index_from}. *)

  val rindex_from : java'lang'String java_instance -> int -> int -> int
  (** Similar to {!String.rindex_from}. *)

  val contains : java'lang'String java_instance -> int -> bool
  (** Similar to {!String.contains}. *)

  val contains_from : java'lang'String java_instance -> int -> int -> bool
  (** Similar to {!String.contains_from}. *)

  val rcontains_from : java'lang'String java_instance -> int -> int -> bool
  (** Similar to {!String.rcontains_from}. *)

  val uppercase : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.uppercase}. *)

  val lowercase : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.lowercase}. *)

  val capitalize : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.capitalize}. *)

  val uncapitalize : java'lang'String java_instance -> java'lang'String java_instance
  (** Similar to {!String.uncapitalize}. *)

  type t = java'lang'String java_instance
  (** Similar to {!String.t}. *)

  val compare : t -> t -> int
  (** Similar to {!String.compare}. *)

  val compare_ignore_case : t -> t -> int
  (** Similar to {!String.compare}, but ignoring case when comparing
      strings. *)

end
(** Module providing functions that can be used as drop-in replacements
    for function from {!String}. *)


(** {6 String operations} *)

type t = java'lang'String java_instance
(** The type of strings. *)

val char_at : t -> java_int -> java_char
(** Returns the character at the passed index; see
    {java java.lang.String#charAt(int)}.

    @raise Java_exception if index is invalid *)

val length : t -> java_int
(** Returns the length of the passed string; see
    {java java.lang.String#length()}. *)

val is_empty : t -> bool
(** Tests whether the passed string is empty; see
    {java java.lang.String#isEmpty()}. *)

val trim : t -> t
(** Returns a copy of the passed string, without leading and trailing
    spaces; see {java java.lang.String#trim()}. *)

val split : t -> t -> java'lang'String java_instance java_reference_array
(** [split s regexp] splits [s] using regular expression [regexp]; see
    {java java.lang.String#split(java.lang.String)}.

    @raise Java_exception if [regexp] is invalid*)

val matches : t -> t -> bool
(** [matches s regexp] tests whether [s] matches regular expression
    [regexp]; see {java java.lang.String#matches(java.lang.String)}.

    @raise Java_exception if [regexp] is invalid*)

val index_of : t -> t -> java_int
(** [index_of s sub] returns the index of the first occurrence of [sub]
    in [s] if any, [-1l] otherwise; see
    {java java.lang.String#indexOf(java.lang.String)}. *)

val last_index_of : t -> t -> java_int
(** [last_index_of s sub] returns the index of the last occurrence of
    [sub] in [s] if any, [-1l] otherwise; see
    {java java.lang.String#lastIndexOf(java.lang.String)}. *)

val starts_with : t -> t -> bool
(** [starts_with s prefix] tests whether [s] starts with [prefix]; see
    {java java.lang.String#startsWith(java.lang.String)}. *)

val ends_with : t -> t -> bool
(** [ends_with s suffix] tests whether [s] ends with [suffix]; see
    {java java.lang.String#endsWith(java.lang.String)}. *)

val substring : t -> java_int -> java_int -> t
(** [substring s start_idx end_idx] returns the substring of [s]
    beginning at index [start_idx] (inclusive) and ending at index
    [end_idx] (exclusive); see
    {java java.lang.String#substring(int, int)}. *)

val to_char_array : t -> java_char java_char_array
(** Converts the passed string into an array of characters; see
    {java java.lang.String#toCharArray()}. *)

val to_lower_case : t -> t
(** Returns a copy of the passed string with all characters converted to
    lower case; see {java java.lang.String#toLowerCase()}. *)

val to_upper_case : t -> t
(** Returns a copy of the passed string with all characters converted to
    upper case; see {java java.lang.String#toUpperCase()}. *)

val equals : t -> t -> bool
(** Tests whether the passed strings are equal; see
    {java java.lang.String#equals(java.lang.Object)}. *)

val equals_ignore_case : t -> t -> bool
(** Similar to {!equals}, but ignoring case when comparing strings; see
    {java java.lang.String#equalsIgnoreCase(java.lang.String)}. *)

val compare_to : t -> t -> java_int
(** Compares the passed strings; see
    {java java.lang.String#compareTo(java.lang.String)}. *)

val compare_to_ignore_case : t -> t -> java_int
(** Similar to {!compare_to}, but ignoring case when comparing strings;
    see {java java.lang.String#compareToIgnoreCase(java.lang.String)}. *)


(** {6 Conversion from/to OCaml strings} *)

external of_string : string -> java'lang'String java_instance =
  "ocamljava_javastring_of_string"
(** [of_string s] converts the OCaml string [s] into a Java string. *)

external to_string : java'lang'String java_instance -> string =
  "ocamljava_javastring_to_string"
(** [to_string s] converts the Java string [s] into an OCaml string.

    @raise Java_exeption if [s] is [null] *)


(** {6 Null value} *)

val null : java'lang'String java_instance
(** The [null] value. *)

external is_null : java'lang'String java_instance -> bool =
  "java is_null"
(** [is_null x] returns [true] iff [x] is equal to [null]. *)

external is_not_null : java'lang'String java_instance -> bool =
  "java is_not_null"
(** [is_not_null x] returns [false] iff [x] is equal to [null]. *)


(** {6 Output functions} *)

external print_string : java'lang'String java_instance -> unit =
  "ocamljava_javastring_print_string"
(** [print_string s] prints [s] onto the standard ouput. *)

external print_endline : java'lang'String java_instance -> unit =
  "ocamljava_javastring_print_endline"
(** [print_endline s] prints [s] followed by a newline character onto the
    standard ouput. *)

external print_newline : unit -> unit =
  "ocamljava_javastring_print_newline"
(** [print_newline ()] prints a newline character onto the standard ouput. *)

external prerr_string : java'lang'String java_instance -> unit =
  "ocamljava_javastring_prerr_string"
(** [prerr_string s] prints [s] onto the error ouput. *)

external prerr_endline : java'lang'String java_instance -> unit =
  "ocamljava_javastring_prerr_endline"
(** [prerr_endline s] prints [s] followed by a newline character onto the
    error ouput. *)

external prerr_newline : unit -> unit =
  "ocamljava_javastring_prerr_newline"
(** [prerr_newline ()] prints a newline character onto the error ouput. *)

external output_string : out_channel -> java'lang'String java_instance -> unit =
  "ocamljava_javastring_output_string"
(** [output_string ch s] prints [s] onto channel [ch]. *)


(** {6 Input functions} *)

external read_line : unit -> java'lang'String java_instance =
  "ocamljava_javastring_read_line"
(** [read_line ()] reads a line from the standard input.
    Returns [null] when end of input is reached. *)

external input_line : in_channel -> java'lang'String java_instance =
  "ocamljava_javastring_input_line"
(** [input_line ch] reads a line from channel [ch].
    Returns [null] when end of input is reached. *)


(** {6 Miscellaneous} *)

val wrap : java'lang'String java_instance -> java'lang'String java_instance option
(** [wrap x] wraps the reference [x] into an option type:
    - [Some x] if [x] is not [null];
    - [None] if [x] is [null]. *)

val unwrap : 'a java_instance option -> 'a java_instance
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
