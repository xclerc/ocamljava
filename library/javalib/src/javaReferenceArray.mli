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

(** Support for [pack.Class[]] and [prim[]...[]] types. *)


type 'a t = 'a java_reference_array
(** The type of arrays. *)


(** {6 Usual operations} *)

external length : 'a java_reference_array -> int32 =
  "java array length reference"
(** [length a] returns the length of [a].

    @raise Java_exception if [a] is [null] *)

external get : 'a java_reference_array -> int32 -> 'a =
  "java array get reference"
(** [get a i] returns the element at index [i] in [a].

    @raise Java_exception if [a] is [null], or [i] is out of bounds *)

external set : 'a java_reference_array -> int32 -> 'a -> unit =
  "java array set reference"
(** [set a i x] changes the element at index [i] in [a] to [x].

    @raise Java_exception if [a] is [null], or [i] is out of bounds *)

val blit : 'a java_reference_array -> int32 -> 'a java_reference_array -> int32 -> int32 -> unit
(** [blit src srcofs dst dstofs len] copies [len] elements from [src] at
    offset [srcofs] to [dst] at offset [dstofs].

    @raise Java_exception if either [src] or [ofs] is [null] *)

val iter : ('a -> unit) -> 'a java_reference_array -> unit
(** [iter f a] applies [f] to each element of [a].

    @raise Java_exception if [a] is [null] *)

val iteri : (int32 -> 'a -> unit) -> 'a java_reference_array -> unit
(** [iter f a] applies [f] to each element of [a] (also passing element index).

    @raise Java_exception if [a] is [null] *)


val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b java_reference_array -> 'a
(** [fold_left f z a] returns [f (... (f (f z a_0) a_1))] where [a_i] is
    the element of [a] at index [i].

    @raise Java_exception if [a] is [null] *)

val fold_right : ('a -> 'b -> 'b) -> 'a java_reference_array -> 'b -> 'b
(** [fold_right f a z] returns [f a_0 (f a_1 (f ... z))] where [a_i] is
    the element of [a] at index [i].

    @raise Java_exception if [a] is [null] *)


(** {6 Java operations} *)

external to_object : 'a java_reference_array -> java'lang'Object java_instance =
  "java array to_object reference"
(** [to_object a] casts [a] to a bare object. *)

external of_object : java'lang'Object java_instance -> 'a java_reference_array =
  "java array of_object reference"
(** [of_object o] casts object [o] to array.

    @raise Java_exception if cast fails *)

val null : 'a java_reference_array
(** The [null] value. *)

external is_null : 'a java_reference_array -> bool =
  "java is_null"
(** [is_null x] returns [true] iff [x] is equal to [null]. *)

external is_not_null : 'a java_reference_array -> bool =
  "java is_not_null"
(** [is_not_null x] returns [false] iff [x] is equal to [null]. *)

val wrap : 'a java_reference_array -> 'a java_reference_array option
(** [wrap x] wraps the array [x] into an option type:
    - [Some x] if [x] is not [null];
    - [None] if [x] is [null]. *)

val unwrap : 'a java_reference_array option -> 'a java_reference_array
(** [unwrap x] unwraps the option [x] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
