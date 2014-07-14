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

type t

external await : t -> unit =
  "ocamljava_condition_await"

external await_time : t -> int64 -> TimeUnit.t -> bool =
  "ocamljava_condition_await_time"

external await_nanos : t -> int64 -> int64 =
  "ocamljava_condition_await_nanos"

external await_uninterruptibly : t -> unit =
  "ocamljava_condition_await_uninterruptibly"

external signal : t -> unit =
  "ocamljava_condition_signal"

external signal_all : t -> unit =
  "ocamljava_condition_signal_all"
