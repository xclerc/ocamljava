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

(** Map/reduce computations. *)

module type Computation = sig
  type input
  (** The type of input values. *)

  type key
  (** The type of keys. *)

  type value
  (** The type of values. *)

  type output
  (** The type of output values. *)

  val compare_keys : key -> key -> int
  (** Ordering over keys. *)

  val map : input -> (key * value) list
  (** {i map} operation, turning an input value into a list of key/value
      couples. The various calls to [map] are done in parallel by pool
      threads. *)

  val combine : key -> value -> value -> value
  (** {i combine} operation, turning two values into one for a given key.
      The calls to [combine] are done sequentially by the main thread as
      soon as several values are available for a given key. *)

  val reduce : key -> value -> output -> output
  (** {i reduce} operation, folding over all key/value couple in order to
      produce the final result. *)
end
(** Description of a computation. *)

module type S = sig
  type input
  (** The type of input values. *)

  type output
  (** The type of output values. *)

  val compute : ThreadPoolExecutor.t -> input Stream.t -> output -> output
  (** Iterates over the passed stream using pool threads to execute
      {i map} operations over the various input values, and applies
      {i combine} operation over values having identical keys as soon as
      they are available.

      Then, folds over key/value couple through the {i reduce} operation,
      using the third parameter as the inital value.

      Raises [Runtime.Raise] if a {i map}, {i reduce}, {i combine}
      operation, or comparison raises and exception. *)
end
(** Signature of a map/reduce computation. *)

module Make (C : Computation) : S with type input = C.input and type output = C.output
(** Builds a map/reduce implementation for the passed computation. *)
