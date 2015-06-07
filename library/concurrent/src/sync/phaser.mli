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

(** Flexible synchronization abstractions, subsuming countdown latches
    and cyclic barriers. *)


type t = java'util'concurrent'Phaser java_instance
(** The type of phasers, differing from coutdown latches and cyclic
    barriers by the fact that parties explicitly register.

    Phasers are also organized in a tree-like structure, to reduce
    contention: a phaser is automatically registered/deregistered with
    its parent when the number of parties becomes respectively
    non-zero/zero.

    The phase number of a phaser starts at zero, and advances when all
    parties arrive at the phaser. *)

val make : ?parent:t -> ?parties:int32 -> unit -> t
(** [make ~parent:p ~parties:n] returns a new phaser with parent [p]
    (defaulting to [null]), and number of parties [n] (defaulting to
    [0l]); see
    {java java.util.concurrent.Phaser#Phaser(java.util.concurrent.Phaser, int)}.

    @raise Java_exception if [n] is negative *)

val arrive : t -> java_int
(** Records that a party has arrived to the phaser without waiting for
    others, returns the phase number (negative if the phaser is
    terminated); see {java java.util.concurrent.Phaser#arrive()}.

    @raise Java_exception if the phase number would be negative while
                          the phaser is not terminated *)

val arrive_and_await_advance : t -> java_int
(** Records that a party has arrived to the phaser and waits for others,
    returns the phase number (negative if the phaser is terminated); see
    {java java.util.concurrent.Phaser#arriveAndAwaitAdvance()}.

    @raise Java_exception if the phase number would be negative while
                          the phaser is not terminated *)

val arrive_and_deregister : t -> java_int
(** Records that a party has arrived to the phaser without waiting for
    others, deregisters a party, returns the phase number (negative if
    the phaser is terminated); see
    {java java.util.concurrent.Phaser#arriveAndDeregister()}.

    @raise Java_exception if the phase number would be negative while
                          the phaser is not terminated *)

val await_advance : t -> java_int -> java_int
(** Waits for the phase number of the phaser to reach the passed value; see
    {java java.util.concurrent.Phaser#awaitAdvance(int)}. *)

val await_advance_interruptibly : t -> java_int -> java_int
(** Similar to {!await_advance} except that the thread can be interrupted; see
    {java java.util.concurrent.Phaser#awaitAdvanceInterruptibly(int)}.

    @raise Java_exception if the thread is interrupted *)

val await_advance_interruptibly_time : t -> java_int -> java_long -> TimeUnit.t -> java_int
(** [await_advance_interruptibly_time p pn t u] is similar to
    [await_advance_interruptibly p pn], except that the current
    thread will at most wait for [t] (time value whose unit is [u]); see
    {java java.util.concurrent.Phaser#awaitAdvanceInterruptibly(int, long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted
    @raise Java_exception if time has elapsed without reaching the
                          given phase number*)

val bulk_register : t -> java_int -> java_int
(** [bulk_register p n] adds [n] unarrived parties to phaser [p]; see
    {java java.util.concurrent.Phaser#bulkRegister(int)}.

    @raise Java_exception if [n] is negative
    @raise Java_exception if the maximum number of parties has
                          already been reached *)

val force_termination : t -> unit
(** Forces termination of the phaser, includind children phasers; see
    {java java.util.concurrent.Phaser#forceTermination()}. *)

val get_arrived_parties : t -> java_int
(** Returns the number of registered parties that have arrived to the
    phaser; see {java java.util.concurrent.Phaser#getArrivedParties()}. *)

val get_parent : t -> t
(** Returns the parent of the phaser; see
    {java java.util.concurrent.Phaser#getParent()}. *)

val get_phase : t -> java_int
(** Returns the phase number; see
    {java java.util.concurrent.Phaser#getPhase()}. *)

val get_registered_parties : t -> java_int
(** Returns the number of registered parties; see
    {java java.util.concurrent.Phaser#getRegisteredParties()}. *)

val get_root : t -> t
(** Returns the root that can be reached from the phaser by recursively
    visiting parents. Returns the passed phaser if it has no parent; see
    {java java.util.concurrent.Phaser#getRoot()}. *)

val get_unarrived_parties : t -> java_int
(** Returns the number of registered parties that have not yet arrived to
    the phaser; see
    {java java.util.concurrent.Phaser#getUnarrivedParties()}. *)

val is_terminated : t -> bool
(** Tests whether the phaser has been terminated; see
    {java java.util.concurrent.Phaser#isTerminated()}. *)

val register : t -> java_int
(** Adds a new unarrived party to the phaser, and returns the current
    phase number; see {java java.util.concurrent.Phaser#register()}.

    @raise Java_exception if the maximum number of parties has already
                          been reached *)


(** {6 Null value} *)

val null : t
(** The [null] value. *)

external is_null : t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : t -> t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : t option -> t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
