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


(* Instance creation *)

type t = java'util'Random java_instance

let make () =
  Java.make "java.util.Random()" ()

let make_of_seed seed =
  Java.make "java.util.Random(long)" seed

let make_secure ?(algorithm = JavaString.null) ?(provider = JavaString.null) () =
  if Java.is_not_null algorithm then begin
    if Java.is_not_null provider then
      Java.call "java.security.SecureRandom.getInstance(String,String)"
        algorithm provider
      |> Java.cast "java.util.Random"
    else
      Java.call "java.security.SecureRandom.getInstance(String)"
        algorithm
      |> Java.cast "java.util.Random"
  end else
    Java.make "java.security.SecureRandom()" ()
    |> Java.cast "java.util.Random"

let current_thread_local () =
  Java.call "java.util.concurrent.ThreadLocalRandom.current()" ()
  |> Java.cast "java.util.Random"


(* Numbers generation *)

let next_boolean random =
  Java.call "java.util.Random.nextBoolean()" random

let next_bytes random bytes =
  Java.call "java.util.Random.nextBytes(byte[])" random bytes

let next_double random =
  Java.call "java.util.Random.nextDouble()" random

let next_float random =
  Java.call "java.util.Random.nextFloat()" random

let next_gaussian random =
  Java.call "java.util.Random.nextGaussian()" random

let next_int random =
  Java.call "java.util.Random.nextInt()" random

let next_int_bound random bound =
  Java.call "java.util.Random.nextInt(int)" random bound

let next_long random =
  Java.call "java.util.Random.nextLong()" random


(* Generator seed *)

let set_seed random seed =
  Java.call "java.util.Random.setSeed(long)" random seed


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : t -> bool =
  "java is_null"

external is_not_null : t -> bool =
  "java is_not_null"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
