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

(** Atomic containers for values. *)


type 'a t
(** The type of atomic containers for values.

    {b WARNING:} physical comparison is used by the container.

    {b CONSEQUENCE 1:} should be used with caution to store [int32],
    [int64], [nativeint], or [double] values as they are wrapped into
    blocks. Hence, comparisons are done on block addresses rather than
    on wrapped values.

    {b CONSEQUENCE 2:} as OCaml-Java uses boxed values for [int] values,
    the container should not be used to store [int] values.

    Any other type can be safely stored (caching of {i some} [int] values
    ensure that sum types are correctly handled). *)

external make : 'a -> 'a t =
  "ocamljava_atomicreference_make"
(** Returns a new container holding the passed value. *)

external compare_and_set : 'a t -> 'a -> 'a -> bool =
  "ocamljava_atomicreference_compare_and_set"
(** [compare_and_set a e u] atomically sets the value of [a] to [u] if
    the current value is [e]. Returns whether the value of [a] was equal
    to [e]. *)

external get : 'a t -> 'a =
  "ocamljava_atomicreference_get"
(** Returns the current value. *)

external get_and_set : 'a t -> 'a -> 'a =
  "ocamljava_atomicreference_get_and_set"
(** [get_and_set a x] atomically sets the value of [a] to [x], and
    returns the previous value. *)

external lazy_set : 'a t -> 'a -> unit =
  "ocamljava_atomicreference_lazy_set"
(** [lazy_set a x] eventually sets the value of [a] to [x]. *)

external set : 'a t -> 'a -> unit =
  "ocamljava_atomicreference_set"
(** [set a x] sets the value of [a] to [x]. *)

external weak_compare_and_set : 'a t -> 'a -> 'a -> bool =
  "ocamljava_atomicreference_weak_compare_and_set"
(** Similar to [compare_and_set], with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees. *)
