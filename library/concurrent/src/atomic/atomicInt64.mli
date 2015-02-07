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

(** Atomic containers for [int64] values. *)


type t
(** The type of atomic containers for [int64] values. *)

external make : int64 -> t =
  "ocamljava_atomicint64_make"
(** Returns a new container holding the passed value. *)

external add_and_get : t -> int64 -> int64 =
  "ocamljava_atomicint64_add_and_get"
(** [add_and_get a d] atomically adds [d] to the current value, and
    returns the new value. *)

external compare_and_set : t -> int64 -> int64 -> bool =
  "ocamljava_atomicint64_compare_and_set"
(** [compare_and_set a e u] atomically sets the value of [a] to [u] if
    the current value is [e]. Returns whether the value of [a] was equal
    to [e]. *)

external decrement_and_get : t -> int64 =
  "ocamljava_atomicint64_decrement_and_get"
(** Atomically decrements the current value, and returns the new value. *)

external get : t -> int64 =
  "ocamljava_atomicint64_get"
(** Returns the current value. *)

external get_and_add : t -> int64 -> int64 =
  "ocamljava_atomicint64_get_and_add"
(** [get_and_add a d] atomically adds [d] to the current value, and
    returns the previous value. *)

external get_and_decrement : t -> int64 =
  "ocamljava_atomicint64_get_and_decrement"
(** Atomically decrements the current value, and returns the previous
    value. *)

external get_and_increment : t -> int64 =
  "ocamljava_atomicint64_get_and_increment"
(** Atomically increments the current value, and returns the previous
    value. *)

external get_and_set : t -> int64 -> int64 =
  "ocamljava_atomicint64_get_and_set"
(** [get_and_set a x] atomically sets the value of [a] to [x], and
    returns the previous value. *)

external increment_and_get : t -> int64 =
  "ocamljava_atomicint64_increment_and_get"
(** Atomically increments the current value, and returns the new value. *)

external lazy_set : t -> int64 -> unit =
  "ocamljava_atomicint64_lazy_set"
(** [lazy_set a x] eventually sets the value of [a] to [x]. *)

external set : t -> int64 -> unit =
  "ocamljava_atomicint64_set"
(** [set a x] sets the value of [a] to [x]. *)

external weak_compare_and_set : t -> int64 -> int64 -> bool =
  "ocamljava_atomicint64_weak_compare_and_set"
(** Similar to [compare_and_set], with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees. *)
