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

(** $(module_ocamldoc) *)


(** {6 Instance creation} *)

type t = $(ocaml_full_name) java_instance
(** $(type_ocamldoc) *)

val make : ?cause:t -> ?message:JavaString.t -> unit -> t
(** Returns a new {java $(java_full_name)} instance;
    see {java $(java_full_name)#$(java_short_name)(java.lang.String, java.lang.Throwable)}. *)


(** {6 Throw} *)

external throw : t -> 'a = "java throw"
(** Synonym for {!Java.throw}. *)


(** {6 Base methods} *)

val get_cause : t -> java'lang'Throwable java_instance
(** Returns the case; see {java java.lang.Throwable#getCause()}. *)

val get_message : t -> JavaString.t
(** Returns the message; see {java java.lang.Throwable#getMessage()}. *)

val get_stack_trace : t -> java'lang'StackTraceElement java_instance java_reference_array
(** Returns the stack trace; see {java java.lang.Throwable#getStackTrace()}. *)

val print_stack_trace : t -> unit
(** Returns the stack trace; see {java java.lang.Throwable#printStackTrace()}. *)


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
