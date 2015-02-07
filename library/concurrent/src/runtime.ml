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


(* Time *)

external current_time_millis : unit -> int64 =
  "ocamljava_runtime_current_time_millis"

external nano_time : unit -> int64 =
  "ocamljava_runtime_nano_time"


(* Processors *)

external available_processors : unit -> int32 =
  "ocamljava_runtime_available_processors"


(* Memory *)

external free_memory : unit -> int64 =
  "ocamljava_runtime_free_memory"

external max_memory : unit -> int64 =
  "ocamljava_runtime_max_memory"

external total_memory : unit -> int64 =
  "ocamljava_runtime_total_memory"


(* Exceptions *)

exception Interrupted of string

let () = Callback.register_exception "Concurrent.Runtime.Interrupted" (Interrupted "")

exception Timeout of string

let () = Callback.register_exception "Concurrent.Runtime.Timeout" (Timeout "")

exception Raised of exn

let () = Callback.register_exception "Concurrent.Runtime.Raised" (Raised (Not_found))
