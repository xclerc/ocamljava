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


type 'a t
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

external make : 'a -> stamp -> 'a t =
  "ocamljava_atomicstampedreference_make"
(** Returns a new container holding the passed value, with the passed
    stamp. *)

external attempt_stamp : 'a t -> 'a -> stamp -> bool =
  "ocamljava_atomicstampedreference_attempt_stamp"
(** [attempt_stamp a e s] sets the stamp to [s] if the current value is
    [e]. Returns whether the value of [a] was equal to [e]. *)

external compare_and_set : 'a t -> 'a -> 'a -> stamp -> stamp -> bool =
  "ocamljava_atomicstampedreference_compare_and_set"
(** [compare_and_set a er ur es us] atomically sets the value of [a] to
    [ur] and stamp to [us] if the current value is [er] and the current
    stamp is [es]. Returns whether the value of [a] was equal to [er] and
    the stamp was equal to [es]. *)

external get : 'a t -> 'a * stamp =
  "ocamljava_atomicstampedreference_get"
(** Returns the current value and stamp. *)

external get_reference : 'a t -> 'a =
  "ocamljava_atomicstampedreference_get_reference"
(** Returns the current value. *)

external get_stamp : 'a t -> stamp =
  "ocamljava_atomicstampedreference_get_stamp"
(** Returns the current stamp. *)

external set : 'a t -> 'a -> stamp -> unit =
  "ocamljava_atomicstampedreference_set"
(** [set a x s] sets the value of [a] to [x], and the stamp to [s]. *)

external weak_compare_and_set : 'a t -> 'a -> 'a -> stamp -> stamp -> bool =
  "ocamljava_atomicstampedreference_weak_compare_and_set"
(** Similar to [compare_and_set], with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees. *)
