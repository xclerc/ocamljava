(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Support for [$(java_element_type)[]] type. *)


type e = $(ocaml_element_type)
(** The type of array elements. *)

type 'a t = 'a $(ocaml_java_type)
(** The type of arrays. *)


(** {6 Usual operations} *)

val make : int32 -> e $(ocaml_java_type)
(** [make len] creates and returns an array of [len] elements.
    All elements are set to $(zero).

    @raise Java_exception if [len] is negative *)

val init : int32 -> (int32 -> e) -> e $(ocaml_java_type)
(** [init len f] creates and returns an array of [len] elements.
    The element at index [i] is initialized with value [f i].

    @raise Java_exception if [len] is negative *)

external length : e $(ocaml_java_type) -> int32 =
  "java array length $(java_element_type)"
(** [length a] returns the length of [a].

    @raise Java_exception if [a] is [null] *)

external get : e $(ocaml_java_type) -> int32 -> e =
  "java array get $(java_element_type)"
(** [get a i] returns the element at index [i] in [a].

    @raise Java_exception if [a] is [null], or [i] is out of bounds *)

external set : e $(ocaml_java_type) -> int32 -> e -> unit =
  "java array set $(java_element_type)"
(** [set a i x] changes the element at index [i] in [a] to [x].

    @raise Java_exception if [a] is [null], or [i] is out of bounds *)

val append : e $(ocaml_java_type) -> e $(ocaml_java_type) -> e $(ocaml_java_type)
(** [append a1 a2] returns the concatenation of [a1] and [a2].

    @raise Java_exception if either [a1] or [a2] is [null] *)

val concat : e $(ocaml_java_type) list -> e $(ocaml_java_type)
(** [concat l] returns the concatenation of arrays in [l].

    @raise Java_exception if any of the arrays in [l] is [null] *)

val sub : e $(ocaml_java_type) -> int32 -> int32 -> e $(ocaml_java_type)
(** [sub a ofs len] returns an array of [len] elements, copying elements
    from [a] starting at offset [ofs].

    @raise Java_exception if [a] is [null] *)

val copy : e $(ocaml_java_type) -> e $(ocaml_java_type)
(** [copy a] returns a copy of [a].

    @raise Java_exception if [a] is [null] *)

val fill : e $(ocaml_java_type) -> int32 -> int32 -> e -> unit
(** [fill a ofs len x] sets [len] elements of [a] to [x], starting at
    offset [ofs].

    @raise Java_exception if [a] is [null] *)

val blit : e $(ocaml_java_type) -> int32 -> e $(ocaml_java_type) -> int32 -> int32 -> unit
(** [blit src srcofs dst dstofs len] copies [len] elements from [src] at
    offset [srcofs] to [dst] at offset [dstofs].

    @raise Java_exception if either [src] or [ofs] is [null] *)

val to_list : e $(ocaml_java_type) -> e list
(** [to_list a] returns the elements of [a] as a list.

    @raise Java_exception if [a] is [null] *)

val of_list : e list -> e $(ocaml_java_type)
(** [of_list l] returns the elements of [l] as an array.

    @raise Invalid_argument if [l] has more elements than can be
                            represented by an [int32] value *)

val iter : (e -> unit) -> e $(ocaml_java_type) -> unit
(** [iter f a] applies [f] to each element of [a].

    @raise Java_exception if [a] is [null] *)

val map : (e -> e) -> e $(ocaml_java_type) -> e $(ocaml_java_type)
(** [map f a] returns an array with elements [f a_0, f a_1, ...] where
    [a_i] is the element of [a] at index [i].

    @raise Java_exception if [a] is [null] *)

val iteri : (int32 -> e -> unit) -> e $(ocaml_java_type) -> unit
(** [iter f a] applies [f] to each element of [a] (also passing element index).

    @raise Java_exception if [a] is [null] *)

val mapi : (int32 -> e -> e) -> e $(ocaml_java_type) -> e $(ocaml_java_type)
(** [map f a] returns an array with elements [f 0 a_0, f 1 a_1, ...] where
    [a_i] is the element of [a] at index [i].

    @raise Java_exception if [a] is [null] *)

val fold_left : ('a -> e -> 'a) -> 'a -> e $(ocaml_java_type) -> 'a
(** [fold_left f z a] returns [f (... (f (f z a_0) a_1))] where [a_i] is
    the element of [a] at index [i].

    @raise Java_exception if [a] is [null] *)

val fold_right : (e -> 'a -> 'a) -> e $(ocaml_java_type) -> 'a -> 'a
(** [fold_right f a z] returns [f a_0 (f a_1 (f ... z))] where [a_i] is
    the element of [a] at index [i].

    @raise Java_exception if [a] is [null] *)


(** {6 Conversion from/to OCaml arrays} *)

val of_ocaml : e array -> e $(ocaml_java_type)
(** [of_ocaml a] returns a Java array equivalent to [a].

    @raise Invalid_argument if [a] has more elements than can be
                            represented by an [int32] value *)

val to_ocaml : e $(ocaml_java_type) -> e array
(** [to_ocaml a] returns an OCaml array equivalent to [a].

    @raise Java_exception if [a] is [null] *)


(** {6 Java operations} *)

external to_object : e $(ocaml_java_type) -> java'lang'Object java_instance =
  "java array to_object $(java_element_type)"
(** [to_object a] casts [a] to a bare object. *)

external of_object : java'lang'Object java_instance -> e $(ocaml_java_type) =
  "java array of_object $(java_element_type)"
(** [of_object o] casts object [o] to array.

    @raise Java_exception if cast fails *)

val null : e $(ocaml_java_type)
(** The [null] value. *)

external is_null : e $(ocaml_java_type) -> bool =
  "java is_null"
(** [is_null x] returns [true] iff [x] is equal to [null]. *)

external is_not_null : e $(ocaml_java_type) -> bool =
  "java is_not_null"
(** [is_not_null x] returns [false] iff [x] is equal to [null]. *)

val wrap : e $(ocaml_java_type) -> e $(ocaml_java_type) option
(** [wrap x] wraps the array [x] into an option type:
    - [Some x] if [x] is not [null];
    - [None] if [x] is [null]. *)
val unwrap : e $(ocaml_java_type) option -> e $(ocaml_java_type)
(** [unwrap x] unwraps the option [x] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
$(extra_intf)
