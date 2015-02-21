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

(** Pseudorandom number generators. *)


(** {6 Instance creation} *)

type t = java'util'Random java_instance
(** The type of pseudorandom number generators. *)

val make : unit -> t
(** Returns a new {java java.util.Random} instance. *)

val make_of_seed : java_long -> t
(** Returns a new {java java.util.Random} instance, built using the passed
    seed. *)

val make_secure : ?algorithm:JavaString.t -> ?provider:JavaString.t -> unit -> t
(** Returns a new {java java.security.SecureRandom} instance, using
    [algorithm] (if different from [null]) to generate the numbers as
    implemented by [provider] (if different from [null]).

    Both [algorithm] and [provider] default to [null].

    @raise Java_exception if the specified algorithm or provider cannot be
                          found *)

val current_thread_local : unit -> t
(** Returns the instance of {java java.util.concurrent.ThreadLocalRandom}
    for the current thread. *)


(** {6 Numbers generation} *)

val next_boolean : t -> java_boolean
(** Generates a new pseudorandom boolean; see
    {java java.util.Random#nextBoolean()}. *)

val next_bytes : t -> java_byte java_byte_array -> unit
(** Generates new pseudorandom bytes; see
    {java java.util.Random#nextBytes(byte[])}. *)

  val next_double : t -> java_double
(** Generates a new pseudorandom double; see
    {java java.util.Random#nextDouble()}. *)

val next_float : t -> java_float
(** Generates a new pseudorandom float; see
    {java java.util.Random#nextFloat()}. *)

val next_gaussian : t -> java_double
(** Generates a new pseudorandom double that is Gaussian distributed; see
    {java java.util.Random#nextGaussian()}. *)

val next_int : t -> java_int
(** Generates a new pseudorandom integer; see
    {java java.util.Random#nextInt()}. *)

val next_int_bound : t -> java_int -> java_int
(** Generates a new pseudorandom integer between 0 (inclusive) and the
    passed value (exclusive); see {java java.util.Random#nextInt(int)}. *)

val next_long : t -> java_long
(** Generates a new pseudorandom long; see
    {java java.util.Random#nextLong()}. *)


(** {6 Generator seed} *)

val set_seed : t -> java_long -> unit
(** [set_seed random seed] Sets the seed of the [random] generator to
    [seed]; see {java java.util.Random#setSeed(long)}. *)


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
