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

(** Flexible synchronization abstractions, subsuming countdown latches
    and cyclic barriers. *)


type t
(** The type of phasers, differing from coutdown latches and cyclic
    barriers by the fact that parties explicitly register.

    Phasers are also organized in a tree-like structure, to reduce
    contention: a phaser is automatically registered/deregistered with
    its parent when the number of parties becomes respectively
    non-zero/zero.

    The phase number of a phaser starts at zero, and advances when all
    parties arrive at the phaser. *)

external make : t option -> int32 -> t =
  "ocamljava_phaser_make"
(** [make p n] returns a new phaser with parent [p], and number of
    parties [n].

    Raises [Invalid_argument] if [n] is negative. *)

external arrive : t -> int32 =
  "ocamljava_phaser_arrive"
(** Records that a party has arrived to the phaser without waiting for
    others, returns the phase number (negative if the phaser is
    terminated).

    Raises [Invalid_argument] if the phase number would be negative while
    the phaser is not terminated. *)

external arrive_and_await_advance : t -> int32 =
  "ocamljava_phaser_arrive_and_await_advance"
(** Records that a party has arrived to the phaser and waits for others,
    returns the phase number (negative if the phaser is terminated).

    Raises [Invalid_argument] if the phase number would be negative while
    the phaser is not terminated. *)

external arrive_and_deregister : t -> int32 =
  "ocamljava_phaser_arrive_and_deregister"
(** Records that a party has arrived to the phaser without waiting for
    others, deregisters a party, returns the phase number (negative if
    the phaser is terminated).

    Raises [Invalid_argument] if the phase number would be negative while
    the phaser is not terminated. *)

external await_advance : t -> int32 -> int32 =
  "ocamljava_phaser_await_advance"
(** Waits for the phase number of the phaser to reach the passed value. *)

external await_advance_interruptibly : t -> int32 -> int32 =
  "ocamljava_phaser_await_advance_interruptibly"
(** Similar to [await_advance] except that the thread can be interrupted.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external await_advance_interruptibly_time : t -> int32 -> int64 -> TimeUnit.t -> int32 =
  "ocamljava_phaser_await_advance_interruptibly_time"
(** [await_advance_interruptibly_time p pn t u] is similar to
    [await_advance_interruptibly p pn], except that the current
    thread will at most wait for [t] (time value whose unit is [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Runtime.Timeout] if time has elapsed without reaching the
    given phase number.*)

external bulk_register : t -> int32 -> int32 =
  "ocamljava_phaser_bulk_register"
(** [bulk_register p n] adds [n] unarrived parties to phaser [p].

    Raises [Invalid_argument] if [n] is negative.

    Raises [Invalid_argument] if the maximum number of parties has
    already been reached. *)

external force_termination : t -> unit =
  "ocamljava_phaser_force_termination"
(** Forces termination of the phaser, includind children phasers. *)

external get_arrived_parties : t -> int32 =
  "ocamljava_phaser_get_arrived_parties"
(** Returns the number of registered parties that have arrived to the
    phaser. *)

external get_parent : t -> t option =
  "ocamljava_phaser_get_parent"
(** Returns the parent of the phaser. *)

external get_phase : t -> int32 =
  "ocamljava_phaser_get_phase"
(** Returns the phase number. *)

external get_registered_parties : t -> int32 =
  "ocamljava_phaser_get_registered_parties"
(** Returns the number of registered parties. *)

external get_root : t -> t =
  "ocamljava_phaser_get_root"
(** Returns the root that can be reached from the phaser by recursively
    visiting parents. Returns the passed phaser if it has no parent. *)

external get_unarrived_parties : t -> int32 =
  "ocamljava_phaser_get_unarrived_parties"
(** Returns the number of registered parties that have not yet arrived to
    the phaser. *)

external is_terminated : t -> bool =
  "ocamljava_phaser_is_terminated"
(** Tests whether the phaser has been terminated. *)

external register : t -> int32 =
  "ocamljava_phaser_register"
(** Adds a new unarrived party to the phaser, and returns the current
    phase number

    Raises [Invalid_argument] if the maximum number of parties has
    already been reached. *)
