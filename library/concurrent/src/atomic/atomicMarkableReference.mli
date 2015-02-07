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

(** Atomic containers for markable values. *)


type 'a t
(** The type of atomic containers for markable values.

    {b WARNING:} physical comparison is used by the container.

    {b CONSEQUENCE 1:} should be used with caution to store [int32],
    [int64], [nativeint], or [double] values as they are wrapped into
    blocks. Hence, comparisons are done on block addresses rather than
    on wrapped values.

    {b CONSEQUENCE 2:} as OCaml-Java uses boxed values for [int] values,
    the container should not be used to store [int] values.

    Any other type can be safely stored (caching of {i some} [int] values
    ensure that sum types are correctly handled). *)

type mark = bool
(** The type of marks. *)

external make : 'a -> mark -> 'a t =
  "ocamljava_atomicmarkablereference_make"
(** Returns a new container holding the passed value, with the passed
    mark. *)

external attempt_mark : 'a t -> 'a -> mark -> bool =
  "ocamljava_atomicmarkablereference_attempt_mark"
(** [attempt_mark a e m] sets the mark to [m] if the current value is
    [e]. Returns whether the value of [a] was equal to [e]. *)

external compare_and_set : 'a t -> 'a -> 'a -> mark -> mark -> bool =
  "ocamljava_atomicmarkablereference_compare_and_set"
(** [compare_and_set a er ur em um] atomically sets the value of [a] to
    [ur] and mark to [um] if the current value is [er] and the current
    mark is [em]. Returns whether the value of [a] was equal to [er] and
    the mark was equal to [em]. *)

external get : 'a t -> 'a * mark =
  "ocamljava_atomicmarkablereference_get"
(** Returns the current value and mark. *)

external get_reference : 'a t -> 'a =
  "ocamljava_atomicmarkablereference_get_reference"
(** Returns the current value. *)

external is_marked : 'a t -> bool =
  "ocamljava_atomicmarkablereference_is_marked"
(** Returns the current mark. *)

external set : 'a t -> 'a -> mark -> unit =
  "ocamljava_atomicmarkablereference_set"
(** [set a x m] sets the value of [a] to [x], and the mark to [m]. *)

external weak_compare_and_set : 'a t -> 'a -> 'a -> mark -> mark -> bool =
  "ocamljava_atomicmarkablereference_weak_compare_and_set"
(** Similar to [compare_and_set], with a {i weak} semantics: may be
    faster on some platforms, but does not provide ordering guarantees. *)
