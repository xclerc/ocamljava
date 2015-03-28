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

open Class'java'util'concurrent'Exchanger

type t = _'Exchanger java_instance

let make () =
  Java.make "Exchanger()" ()

external exchange : t -> 'a -> 'a =
  "ocamljava_exchanger_exchange"

external exchange_time : t -> 'a -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_exchanger_exchange_time"
