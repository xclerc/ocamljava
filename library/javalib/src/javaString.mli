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

val equals : t -> t -> bool
(** Tests whether the passed strings are equal. *)

val equals_ignore_case : t -> t -> bool
(** Similar to {!String.equals}, but ignoring case when comparing
    strings. *)


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

external prerr_string : java'lang'String java_instance -> unit =
  "ocamljava_javastring_prerr_string"
(** [prerr_string s] prints [s] onto the error ouput. *)

external prerr_endline : java'lang'String java_instance -> unit =
  "ocamljava_javastring_prerr_endline"
(** [prerr_endline s] prints [s] followed by a newline character onto the
    error ouput. *)

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
