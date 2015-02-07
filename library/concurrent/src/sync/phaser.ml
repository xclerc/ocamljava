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

external make : t option -> int32 -> t =
  "ocamljava_phaser_make"

external arrive : t -> int32 =
  "ocamljava_phaser_arrive"

external arrive_and_await_advance : t -> int32 =
  "ocamljava_phaser_arrive_and_await_advance"

external arrive_and_deregister : t -> int32 =
  "ocamljava_phaser_arrive_and_deregister"

external await_advance : t -> int32 -> int32 =
  "ocamljava_phaser_await_advance"

external await_advance_interruptibly : t -> int32 -> int32 =
  "ocamljava_phaser_await_advance_interruptibly"

external await_advance_interruptibly_time : t -> int32 -> int64 -> TimeUnit.t -> int32 =
  "ocamljava_phaser_await_advance_interruptibly_time"

external bulk_register : t -> int32 -> int32 =
  "ocamljava_phaser_bulk_register"

external force_termination : t -> unit =
  "ocamljava_phaser_force_termination"

external get_arrived_parties : t -> int32 =
  "ocamljava_phaser_get_arrived_parties"

external get_parent : t -> t option =
  "ocamljava_phaser_get_parent"

external get_phase : t -> int32 =
  "ocamljava_phaser_get_phase"

external get_registered_parties : t -> int32 =
  "ocamljava_phaser_get_registered_parties"

external get_root : t -> t =
  "ocamljava_phaser_get_root"

external get_unarrived_parties : t -> int32 =
  "ocamljava_phaser_get_unarrived_parties"

external is_terminated : t -> bool =
  "ocamljava_phaser_is_terminated"

external register : t -> int32 =
  "ocamljava_phaser_register"
