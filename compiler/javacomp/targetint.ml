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

type t = int

let zero = 0

let one = 1

let minus_one = -1

let neg x = -x

let add x y = x + y

let sub x y = x - y

let mul x y = x * y

let div x y = x / y

let rem x y = x mod y

let succ x = Pervasives.succ x

let pred x = Pervasives.pred x

let abs x = Pervasives.abs x

let max_int = Pervasives.max_int

let min_int = Pervasives.min_int

let logand x y = x land y

let logor x y = x lor y

let logxor x y = x lxor y

let lognot x = x lxor minus_one

let shift_left x y = x lsl y

let shift_right_logical x y = x lsr y

let shift_right x y = x asr y

let of_int x = x

let to_int x = x

let of_float x = int_of_float x

let to_float x = float_of_int x

let of_int32 x = Int32.to_int x

let to_int32 x = Int32.of_int x

let of_int64 x = Int64.to_int x

let to_int64 x = Int64.of_int x

let of_nativeint x = Nativeint.to_int x

let to_nativeint x = Nativeint.of_int x

let compare (x : t) (y : t) = Pervasives.compare x y

let equal (x : t) (y : t) = x = y

let print ppf x = Format.fprintf ppf "%dt" x

let min x y = Pervasives.min x y

let max x y = Pervasives.max x y

let incr x = x := !x + 1

let decr x = x := !x - 1

let bswap16 x = (((x land 0xff) lsl 8) lor ((x land 0xff00) lsr 8))
