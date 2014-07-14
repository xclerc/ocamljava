(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java compiler is free software; you can redistribute it and/or modify
 * it under the terms of the Q Public License as published by
 * Trolltech (with a change to choice of law).
 *
 * OCaml-Java compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Q Public License for more details.
 *
 * You should have received a copy of the Q Public License
 * along with this program.  If not, see
 * <http://opensource.org/licenses/QPL-1.0>.
 *)

(** Akin to [Int32]/[Int64]/[Nativeint] modules, except that actual bit
    size of values depends on [ints_are_63_bit_long]. *)

type t

val zero : t

val one : t

val minus_one : t

val neg : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val rem : t -> t -> t

val succ : t -> t

val pred : t -> t

val abs : t -> t

val max_int : t

val min_int : t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val lognot : t -> t

val shift_left : t -> t -> t

val shift_right_logical : t -> t -> t

val shift_right : t -> t -> t

val of_int : int -> t

val to_int : t -> int

val of_float : float -> t

val to_float : t -> float

val of_int32 : int32 -> t

val to_int32 : t -> int32

val of_int64 : int64 -> t

val to_int64 : t -> int64

val of_nativeint : nativeint -> t

val to_nativeint : t -> nativeint

val compare : t -> t -> int

val equal : t -> t -> bool

val print : Format.formatter -> t -> unit

val min : t -> t -> t

val max : t -> t -> t

val incr : t ref -> unit

val decr : t ref -> unit

val bswap16 : t -> t
