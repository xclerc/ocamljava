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

(** Utility functions for string builders. *)


(** {6 Instance creation} *)

type t = java'lang'StringBuilder java_instance
(** The type of string builders. *)

val make : unit -> t
(** Returns new empty {java java.lang.StringBuilder} instance, with an
    initial capacity of 16 characters. *)

val make_of_capacity : java_int -> t
(** Returns new empty {java java.lang.StringBuilder} instance, with the
    passed capacity. *)

val make_of_char_sequence : java'lang'CharSequence java_extends -> t
(** Returns a {java java.lang.StringBuilder} instance, containing a copy
    of the passed sequence. *)

val make_of_string : JavaString.t -> t
(** Returns a {java java.lang.StringBuilder} instance, containing a copy
    of the passed string. *)


(** {6 Capacity and length} *)

val capacity : t -> java_int
(** Returns the capacity of the passed builder; see
    {java java.lang.StringBuilder#capacity()}. *)

val ensure_capacity : t -> java_int -> unit
(** Ensures that the passed builder is at least equal to the passed
    value; see {java java.lang.StringBuilder#ensureCapacity(int)}. *)

val length : t -> java_int
(** Returns the length of the passed builder; see
    {java java.lang.StringBuilder#length()}. *)

val set_length : t -> java_int -> unit
(** Sets the length of the passed builder to the passed value; see
    {java java.lang.StringBuilder#setLength(int)}.

    @raise Java_exception if the length is negative *)

val trim_to_size : t -> unit
(** Minimizes the amount of memory used by the passed builder; see
    {java java.lang.StringBuilder#trimToSize()}. *)


(** {6 Value addition} *)

val append_boolean : t -> java_boolean -> t
(** Appends the passed boolean to the passed builder; see
    {java java.lang.StringBuilder#append(boolean)}. *)

val append_char : t -> java_char -> t
(** Appends the passed character to the passed builder; see
    {java java.lang.StringBuilder#append(char)}. *)

val append_double : t -> java_double -> t
(** Appends the passed double to the passed builder; see
    {java java.lang.StringBuilder#append(double)}. *)

val append_float : t -> java_float -> t
(** Appends the passed float to the passed builder; see
    {java java.lang.StringBuilder#append(float)}. *)

val append_int : t -> java_int -> t
(** Appends the passed integer to the passed builder; see
    {java java.lang.StringBuilder#append(int)}. *)

val append_long : t -> java_long -> t
(** Appends the passed long to the passed builder; see
    {java java.lang.StringBuilder#append(long)}. *)

val append_string : t -> JavaString.t -> t
(** Appends the passed string to the passed builder; see
    {java java.lang.StringBuilder#append(java.lang.String)}. *)

val append_code_point : t -> java_int -> t
(** Appends the passed code point to the passed builder; see
    {java java.lang.StringBuilder#appendCodePoint(int)}. *)


(** {6 Value insertion} *)

val insert_boolean : t -> java_int -> java_boolean -> t
(** Inserts the passed boolean into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, boolean)}.

    @raise Java_exception if the index is invalid *)

val insert_char : t -> java_int -> java_char -> t
(** Inserts the passed character into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, char)}.

    @raise Java_exception if the index is invalid *)

val insert_double : t -> java_int -> java_double -> t
(** Inserts the passed double into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, double)}.

    @raise Java_exception if the index is invalid *)

val insert_float : t -> java_int -> java_float -> t
(** Inserts the passed float into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, float)}.

    @raise Java_exception if the index is invalid *)

val insert_int : t -> java_int -> java_int -> t
(** Inserts the passed integer into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, int)}.

    @raise Java_exception if the index is invalid *)

val insert_long : t -> java_int -> java_long -> t
(** Inserts the passed long into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, long)}.

    @raise Java_exception if the index is invalid *)

val insert_string : t -> java_int -> JavaString.t -> t
(** Inserts the passed string into the passed builder at the specified
    index; see {java java.lang.StringBuilder#insert(int, java.lang.String)}.

    @raise Java_exception if the index is invalid *)


(** {6 Character lookup and modification} *)

val char_at : t -> java_int -> java_char
(** Returns the character at the passed index in the passed builder; see
    {java java.lang.StringBuilder#charAt(int)}.

    @raise Java_exception if the index is invalid *)

val set_char_at : t -> java_int -> java_char -> unit
(** Changes the character at the passed index in the passed builder; see
    {java java.lang.StringBuilder#setCharAt(int, char)}.

    @raise Java_exception if the index is invalid *)

val code_point_at : t -> java_int -> java_int
(** Returns the code point at the passed index in the passed builder; see
    {java java.lang.StringBuilder#codePointAt(int)}.

    @raise Java_exception if the index is invalid *)


(** {6 Deletion} *)

val delete : t -> java_int -> java_int -> t
(** [delete sb start end] deletes the characters from index [start]
    (inclusive) to index [end] (exclusive) from builder [sb]; see
    {java java.lang.StringBuilder#delete(int, int)}.

    @raise Java_exception if the indexes are invalid *)

val delete_char_at : t -> java_int -> t
(** [delete_char_at sb idx] deletes the characters at index [idx] from
    builder [sb]; see {java java.lang.StringBuilder#deleteCharAt(int)}.

    @raise Java_exception if the index is invalid *)


(** {6 Replacement} *)

val replace : t -> java_int -> java_int -> JavaString.t -> t
(** [replace sb start end str] replaces the characters from index [start]
    (inclusive) to index [end] (exclusive) from builder [sb] by
    characters from [str]; see
    {java java.lang.StringBuilder#replace(int, int, java.lang.String)}.

    @raise Java_exception if the indexes are invalid *)


(** {6 Search} *)

val index_of : t -> JavaString.t -> java_int
(** Returns the index within the passed builder of the first occurrence
    of the passed string; see
    {java java.lang.StringBuilder#indexOf(java.lang.String)}. *)

val index_of_from : t -> JavaString.t -> java_int -> java_int
(** Returns the index within the passed builder of the first occurrence
    of the passed string, starting at the passed index; see
    {java java.lang.StringBuilder#indexOf(java.lang.String, int)}. *)

val last_index_of : t -> JavaString.t -> java_int
(** Returns the index within the passed builder of the last occurrence
    of the passed string; see
    {java java.lang.StringBuilder#lastIndexOf(java.lang.String)}. *)

val last_index_of_from : t -> JavaString.t -> java_int -> java_int
(** Returns the index within the passed builder of the last occurrence
    of the passed string, starting at the passed index; see
    {java java.lang.StringBuilder#indexOf(java.lang.String, int)}. *)


(** {6 Conversion} *)

val substring : t -> java_int -> java_int -> JavaString.t
(** [substring sb start end] returns a string containing the characters
    of [sb] from index [start] (inclusive) to index [end] (exclusive);
    see {java java.lang.StringBuilder#substring(int, int)}.

    @raise Java_exception if the indexes are invalid *)

val to_string : t -> JavaString.t
(** Returns the contents of the passed builder as a string; see
    {java java.lang.StringBuilder#toString()}. *)


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
