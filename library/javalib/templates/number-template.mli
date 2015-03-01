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

(** Support for [$(java_primitive)] type and [$(java_wrapper)] class. *)


(** {6 Instance creation} *)

type t = java'lang'$(java_wrapper) java_instance
(** The type of wrappers for $(name) values. *)

val min_value : $(ocaml_type)
(** The smallest value for $(name) values. *)
    
val max_value : $(ocaml_type)
(** The largest value for $(name) values. *)

$(extra_constants_intf)

val make : $(ocaml_type) -> t
(** Returns a new {java java.lang.$(java_wrapper)} wrapper for the passed
    value. *)

val make_of_string : JavaString.t -> t
(** Returns a new {java java.lang.$(java_wrapper)} corresponding to the
    passed string; see
    {java java.lang.$(java_wrapper)#$(java_wrapper)(java.lang.String)}.

    @raise Java_exception if the string is invalid *)


(** {6 Operations} *)

val byte_value : t -> java_byte
(** Returns the wrapped value, as a byte. *)

val double_value : t -> java_double
(** Returns the wrapped value, as a double. *)

val float_value : t -> java_float
(** Returns the wrapped value, as a float. *)

val int_value : t -> java_int
(** Returns the wrapped value, as an int. *)

val long_value : t -> java_long
(** Returns the wrapped value, as a long. *)

val short_value : t -> java_short
(** Returns the wrapped value, as a short. *)

val compare : $(ocaml_type) -> $(ocaml_type) -> java_int
(** Compares the passed values. *)

val compare_to : t -> t -> java_int
(** Compares the passed values. *)

val to_string : $(ocaml_type) -> JavaString.t
(** Converts the passed $(name) into a string; see
    {java java.lang.$(java_wrapper)#toString($(java_primitive))}. *)

val value_of : $(ocaml_type) -> t
(** Converts the passed bare $(name) into a $(name) wrapper; see
    {java java.lang.$(java_wrapper)#valueOf($(java_primitive))}. *)

$(extra_operations_intf)


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
