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

type t

external make : int32 -> t =
  "ocamljava_cyclicbarrier_make"

external await : t -> int32 =
  "ocamljava_cyclicbarrier_await"

external await_time : t -> int64 -> TimeUnit.t -> int32 =
  "ocamljava_cyclicbarrier_await"

external get_number_waiting : t -> int32 =
  "ocamljava_cyclicbarrier_get_number_waiting"

external get_parties : t -> int32 =
  "ocamljava_cyclicbarrier_get_parties"

external is_broken : t -> bool =
  "ocamljava_cyclicbarrier_is_broken"

external reset : t -> unit =
  "ocamljava_cyclicbarrier_reset"
