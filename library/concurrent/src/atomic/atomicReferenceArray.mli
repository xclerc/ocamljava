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

(** Atomic containers for arrays of values. *)


type 'a t
(** The type of atomic containers for arrays values.

    {b WARNING:} physical comparison is used by the container.

    {b CONSEQUENCE 1:} should be used with caution to store [int32],
    [int64], [nativeint], or [double] values as they are wrapped into
    blocks. Hence, comparisons are done on block addresses rather than
    on wrapped values.

    {b CONSEQUENCE 2:} as OCaml-Java uses boxed values for [int] values,
    the container should not be used to store [int] values.

    Any other type can be safely stored (caching of {i some} [int] values
    ensure that sum types are correctly handled). *)

type index = int32
(** The type of array indices. *)

external make : int32 -> 'a -> 'a t =
  "ocamljava_atomicreferencearray_make"
(** Returns a new container holding an array of passed length.

    Raises [Invalid_argument] if passed length is negative. *)

external compare_and_set : 'a t -> index -> 'a -> 'a -> bool =
  "ocamljava_atomicreferencearray_compare_and_set"
(** [compare_and_set a i e u] atomically sets the value of [a] at index
    [i] to [u] if the current value is [e]. Returns whether the value of
    [a] at index [i] was equal to [e].

    Raises [Invalid_argument] if passed index is invalid. *)

external get : 'a t -> index -> 'a =
  "ocamljava_atomicreferencearray_get"
(** Returns the value at passed index.

    Raises [Invalid_argument] if passed index is invalid. *)

external get_and_set : 'a t -> index -> 'a -> 'a =
  "ocamljava_atomicreferencearray_get_and_set"
(** [get_and_set a i x] atomically sets the value of [a] at index [i] to
    [x], and returns the previous value.

    Raises [Invalid_argument] if passed index is invalid. *)

external lazy_set : 'a t -> index -> 'a -> unit =
  "ocamljava_atomicreferencearray_lazy_set"
(** [lazy_set a i x] eventually sets the value of [a] at index [i] to
    [x].

    Raises [Invalid_argument] if passed index is invalid. *)

external length : 'a t -> int32 =
  "ocamljava_atomicreferencearray_length"
(** Returns the length of the array. *)

external set : 'a t -> index -> 'a -> unit =
  "ocamljava_atomicreferencearray_set"
(** [set a i x] sets the value of [a] at index [i] to [x].

    Raises [Invalid_argument] if passed index is invalid. *)

external weak_compare_and_set : 'a t -> index -> 'a -> 'a -> bool =
  "ocamljava_atomicreferencearray_weak_compare_and_set"
(** Similar to [compare_and_set], with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees.

    Raises [Invalid_argument] if passed index is invalid. *)
