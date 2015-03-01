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

(** Utility functions for [boolean] type and [Boolean] class. *)


(** {6 Instance creation} *)

type t = java'lang'Boolean java_instance
(** The type of wrappers for boolean values. *)

val false_ : t
(** The constant for the wrapper around the [false] value. *)

val true_ : t
(** The constant for the wrapper around the [true] value. *)

val make : java_boolean -> t
(** Returns a new {java java.lang.Boolean} wrapper for the passed value. *)

val make_of_string : JavaString.t -> t
(** Returns a new {java java.lang.Boolean} corresponding to the passed
    string; see {java java.lang.Boolean#Boolean(java.lang.String)}. *)


(** {6 Operations} *)

val boolean_value : t -> java_boolean
(** Returns the wrapped value. *)

val compare : java_boolean -> java_boolean -> java_int
(** Compares the passed values. *)

val compare_to : t -> t -> java_int
(** Compares the passed values. *)

val parse_boolean : JavaString.t -> java_boolean
(** Converts the passed string into a boolean; see
    {java java.lang.Boolean#parseBoolean(java.lang.String)}. *)

val to_string : java_boolean -> JavaString.t
(** Converts the passed boolean into a string; see
    {java java.lang.Boolean#toString(boolean)}. *)

val value_of : java_boolean -> t
(** Converts the passed bare boolean into a boolean wrapper;
    see {java java.lang.Boolean#valueOf(boolean)}. *)

val value_of_string : JavaString.t -> t
(** Converts the passed string into a boolean wrapper;
    see {java java.lang.Boolean#valueOf(java.lang.String)}. *)


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
