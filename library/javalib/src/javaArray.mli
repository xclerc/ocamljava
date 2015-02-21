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

(** Support for {i unspecialized} arrays. *)


type (_, _, _) t
(** The type of unspecialized arrays, that are wrappers around
    specialized ones. The type parameters indicate:
    - the type of array elements;
    - the type of array indexes;
    - the type of the array that is wrapped. *)

val wrap_boolean_array : java_boolean java_boolean_array -> (java_boolean, java_int, java_boolean java_boolean_array) t
(** [wrap_boolean_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_byte_array : java_byte java_byte_array -> (java_byte, java_int, java_byte java_byte_array) t
(** [wrap_byte_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_char_array : java_char java_char_array -> (java_char, java_int, java_char java_char_array) t
(** [wrap_char_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_double_array : java_double java_double_array -> (java_double, java_int, java_double java_double_array) t
(** [wrap_double_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_float_array : java_double java_float_array -> (java_double, java_int, java_double java_float_array) t
(** [wrap_float_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_int_array : java_int java_int_array -> (java_int, java_int, java_int java_int_array) t
(** [wrap_int_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_long_array : java_long java_long_array -> (java_long, java_int, java_long java_long_array) t
(** [wrap_long_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_short_array : java_short java_short_array -> (java_short, java_int, java_short java_short_array) t
(** [wrap_short_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_reference_array : 'a java_reference_array -> ('a, java_int, 'a java_reference_array) t
(** [wrap_reference_array a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_boolean_array2 : java_boolean java_boolean_array java_reference_array -> (java_boolean, java_int * java_int, java_boolean java_boolean_array java_reference_array) t
(** [wrap_boolean_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_byte_array2 : java_byte java_byte_array java_reference_array -> (java_byte, java_int * java_int, java_byte java_byte_array java_reference_array) t
(** [wrap_byte_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_char_array2 : java_char java_char_array java_reference_array -> (java_char, java_int * java_int, java_char java_char_array java_reference_array) t
(** [wrap_byte_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_double_array2 : java_double java_double_array java_reference_array -> (java_double, java_int * java_int, java_double java_double_array java_reference_array) t
(** [wrap_double_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_float_array2 : java_float java_float_array java_reference_array -> (java_float, java_int * java_int, java_float java_float_array java_reference_array) t
(** [wrap_float_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_int_array2 : java_int java_int_array java_reference_array -> (java_int, java_int * java_int, java_int java_int_array java_reference_array) t
(** [wrap_int_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_long_array2 : java_long java_long_array java_reference_array -> (java_long, java_int * java_int, java_long java_long_array java_reference_array) t
(** [wrap_long_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_short_array2 : java_short java_short_array java_reference_array -> (java_short, java_int * java_int, java_short java_short_array java_reference_array) t
(** [wrap_short_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val wrap_reference_array2 : 'a java_reference_array java_reference_array -> ('a, java_int * java_int, 'a java_reference_array java_reference_array) t
(** [wrap_reference_array2 a] wraps the passed specialized array into an
   unspecialized one. *)

val length : (_, _, _) t -> java_int
(** [length a] returns the length of [a].

    @raise Java_exception if [a] is [null] *)

val length_sub : (_, java_int * java_int, _) t -> java_int -> java_int
(** [length_sub a i] returns the length of the sub array at index [i] of
    [a].

    @raise Java_exception if [a] is [null], or if [i] is out of bounds *)

val get : ('e, 'i, _) t -> 'i -> 'e
(** [get a i] returns the element at index [i] in [a].

    @raise Java_exception if [a] is [null], or [i] is out of bounds *)

val set : ('e, 'i, _) t -> 'i -> 'e -> unit
(** [set a i x] changes the element at index [i] in [a] to [x].

    @raise Java_exception if [a] is [null], or [i] is out of bounds *)

val iter : ('a -> unit) -> ('a, _, _) t -> unit
(** [iter f a] applies [f] to the elements of [a].

    @raise Java_exception if [a] is [null] *)

val iteri : ('i -> 'a -> unit) -> ('a, 'i, _) t -> unit
(** [iter f a] applies [f] to the elements of [a].

    @raise Java_exception if [a] is [null] *)

val is_null : (_, _, _) t -> bool
(** [is_null a] returns [true] iff [a] is equal to [null]. *)

val is_not_null : (_, _, _) t -> bool
(** [is_not_null a] returns [false] iff [a] is equal to [null]. *)

val wrap : ('e, 'i, 'r) t -> ('e, 'i, 'r) t option
(** [wrap x] wraps the array [x] into an option type:
    - [Some x] if [x] is not [null];
    - [None] if [x] is [null]. *)

val unwrap : ('e, 'i, 'r) t option -> ('e, 'i, 'r) t
(** [unwrap x] unwraps the option [x] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)

val wrapped : (_, _, 'r) t -> 'r
(** [wrapped a] returns the wrapped array. *)
