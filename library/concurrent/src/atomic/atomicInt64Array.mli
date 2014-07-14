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

(** Atomic containers for arrays of [int64] values. *)


type t
(** The type of atomic containers for arrays of [int64] values. *)

type index = int32
(** The type of array indices. *)

external make : int32 -> t =
  "ocamljava_atomicint64array_make"
(** Returns a new container holding an array of passed length.

    Raises [Invalid_argument] if passed length is negative. *)

external add_and_get : t -> index -> int64 -> int64 =
  "ocamljava_atomicint64array_add_and_get"
(** [add_and_get a i d] atomically adds [d] to the value at index [i],
    and returns the new value.

    Raises [Invalid_argument] if passed index is invalid. *)

external compare_and_set : t -> index -> int64 -> int64 -> bool =
  "ocamljava_atomicint64array_compare_and_set"
(** [compare_and_set a i e u] atomically sets the value of [a] at index
    [i] to [u] if the current value is [e]. Returns whether the value of
    [a] at index [i] was equal to [e].

    Raises [Invalid_argument] if passed index is invalid. *)

external decrement_and_get : t -> index -> int64 =
  "ocamljava_atomicint64array_decrement_and_get"
(** Atomically decrements the value at passed index, and returns the new
    value.

    Raises [Invalid_argument] if passed index is invalid. *)

external get : t -> index -> int64 =
  "ocamljava_atomicint64array_get"
(** Returns the value at passed index.

    Raises [Invalid_argument] if passed index is invalid. *)

external get_and_add : t -> index -> int64 -> int64 =
  "ocamljava_atomicint64array_get_and_add"
(** [get_and_add a i d] atomically adds [d] to the value at index [i],
    and returns the previous value.

    Raises [Invalid_argument] if passed index is invalid. *)

external get_and_decrement : t -> index -> int64 =
  "ocamljava_atomicint64array_get_and_decrement"
(** Atomically decrements the value at passed index, and returns the
    previous value.

    Raises [Invalid_argument] if passed index is invalid. *)

external get_and_increment : t -> index -> int64 =
  "ocamljava_atomicint64array_get_and_increment"
(** Atomically increments the value at passed index, and returns the
    previous value.

    Raises [Invalid_argument] if passed index is invalid. *)

external get_and_set : t -> index -> int64 -> int64 =
  "ocamljava_atomicint64array_get_and_set"
(** [get_and_set a i x] atomically sets the value of [a] at index [i] to
    [x], and returns the previous value.

    Raises [Invalid_argument] if passed index is invalid. *)

external increment_and_get : t -> index -> int64 =
  "ocamljava_atomicint64array_increment_and_get"
(** Atomically increments the value at passed index, and returns the new
    value.

    Raises [Invalid_argument] if passed index is invalid. *)

external lazy_set : t -> index -> int64 -> unit =
  "ocamljava_atomicint64array_lazy_set"
(** [lazy_set a i x] eventually sets the value of [a] at index [i] to
    [x].

    Raises [Invalid_argument] if passed index is invalid. *)

external length : t -> int32 =
  "ocamljava_atomicint64array_length"
(** Returns the length of the array. *)

external set : t -> index -> int64 -> unit =
  "ocamljava_atomicint64array_set"
(** [set a i x] sets the value of [a] at index [i] to [x].

    Raises [Invalid_argument] if passed index is invalid. *)

external weak_compare_and_set : t -> index -> int64 -> int64 -> bool =
  "ocamljava_atomicint64array_weak_compare_and_set"
(** Similar to [compare_and_set], with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees.

    Raises [Invalid_argument] if passed index is invalid. *)
