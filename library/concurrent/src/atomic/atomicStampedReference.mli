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

(** Atomic containers for stamped values. *)


type 'a t = java'util'concurrent'atomic'AtomicStampedReference java_instance
(** The type of atomic containers for stamped values.

    {b WARNING:} physical comparison is used by the container.

    {b CONSEQUENCE 1:} should be used with caution to store [int32],
    [int64], [nativeint], or [double] values as they are wrapped into
    blocks. Hence, comparisons are done on block addresses rather than
    on wrapped values.

    {b CONSEQUENCE 2:} as OCaml-Java uses boxed values for [int] values,
    the container should not be used to store [int] values.

    Any other type can be safely stored (caching of {i some} [int] values
    ensure that sum types are correctly handled). *)

type stamp = int32
(** The type of stamps. *)

val make : 'a -> stamp -> 'a t
(** Returns a new container holding the passed value, with the passed
    stamp. *)

val attempt_stamp : 'a t -> 'a -> stamp -> bool
(** [attempt_stamp a e s] sets the stamp to [s] if the current value is
    [e]. Returns whether the value of [a] was equal to [e]. *)

val compare_and_set : 'a t -> 'a -> 'a -> stamp -> stamp -> bool
(** [compare_and_set a er ur es us] atomically sets the value of [a] to
    [ur] and stamp to [us] if the current value is [er] and the current
    stamp is [es]. Returns whether the value of [a] was equal to [er] and
    the stamp was equal to [es]. *)

val get : 'a t -> 'a * stamp
(** Returns the current value and stamp. *)

val get_reference : 'a t -> 'a
(** Returns the current value. *)

val get_stamp : 'a t -> stamp
(** Returns the current stamp. *)

val set : 'a t -> 'a -> stamp -> unit
(** [set a x s] sets the value of [a] to [x], and the stamp to [s]. *)

val weak_compare_and_set : 'a t -> 'a -> 'a -> stamp -> stamp -> bool
(** Similar to {!compare_and_set}, with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees. *)


(** {6 Null value} *)

val null : 'a t
(** The [null] value. *)

external is_null : 'a t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : 'a t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : 'a t -> 'a t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : 'a t option -> 'a t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
