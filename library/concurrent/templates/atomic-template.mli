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

(** Atomic containers for [$(ocaml_type)] values. *)


type t = $(ocaml_java_type)
(** The type of atomic containers for [$(ocaml_type)] values. *)

val make : $(ocaml_type) -> t
(** Returns a new container holding the passed value. *)

$(add_and_get)

val compare_and_set : t -> $(ocaml_type) -> $(ocaml_type) -> bool
(** [compare_and_set a e u] atomically sets the value of [a] to [u] if
    the current value is [e]. Returns whether the value of [a] was equal
    to [e]. *)

$(decrement_and_get)

val get : t -> $(ocaml_type)
(** Returns the current value. *)

$(get_and_add)

$(get_and_decrement)

$(get_and_increment)

val get_and_set : t -> $(ocaml_type) -> $(ocaml_type)
(** [get_and_set a x] atomically sets the value of [a] to [x], and
    returns the previous value. *)

$(increment_and_get)

val lazy_set : t -> $(ocaml_type) -> unit
(** [lazy_set a x] eventually sets the value of [a] to [x]. *)

val set : t -> $(ocaml_type) -> unit
(** [set a x] sets the value of [a] to [x]. *)

val weak_compare_and_set : t -> $(ocaml_type) -> $(ocaml_type) -> bool
(** Similar to {!compare_and_set}, with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees. *)


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
