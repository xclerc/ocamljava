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

type state =
  | New
  | Runnable
  | Blocked
  | Waiting
  | Timed_waiting
  | Terminated

external get_max_priority : unit -> int32 =
  "ocamljava_thread_get_max_priority"

external get_min_priority : unit -> int32 =
  "ocamljava_thread_get_min_priority"

external get_norm_priority : unit -> int32 =
  "ocamljava_thread_get_norm_priority"

let max_priority = get_max_priority ()

let min_priority = get_min_priority ()

let norm_priority = get_norm_priority ()

type t

external make : ThreadGroup.t option -> string option -> ('a -> unit) -> 'a -> t =
  "ocamljava_thread_make"

external current_thread : unit -> t =
  "ocamljava_thread_current_thread"

external get_id : t -> int64 =
  "ocamljava_thread_get_id"

external get_name : t -> string =
  "ocamljava_thread_get_name"

external get_priority : t -> string =
  "ocamljava_thread_get_priority"

external get_state : t -> state =
  "ocamljava_thread_get_state"

external get_thread_group : t -> ThreadGroup.t option =
  "ocamljava_thread_get_thread_group"

external interrupt : t -> unit =
  "ocamljava_thread_interrupt"

external interrupted : unit -> bool =
  "ocamljava_thread_interrupted"

external is_alive : t -> bool =
  "ocamljava_thread_is_alive"

external is_daemon : t -> bool =
  "ocamljava_thread_is_daemon"

external is_interrupted : t -> bool =
  "ocamljava_thread_is_interrupted"

external join : t -> unit =
  "ocamljava_thread_join"

external join_time : t -> int64 -> unit =
  "ocamljava_thread_join_time"

external join_time_nanos : t -> int64 -> int32 -> unit =
  "ocamljava_thread_join_time_nanos"

external set_daemon : t -> bool -> unit =
  "ocamljava_thread_set_daemon"

external set_name : t -> string -> unit =
  "ocamljava_thread_set_name"

external set_priority : t -> int32 -> unit =
  "ocamljava_thread_set_priority"

external sleep : int64 -> unit =
  "ocamljava_thread_sleep"

external sleep_nanos : int64 -> int32 -> unit =
  "ocamljava_thread_sleep_nanos"

external start : t -> unit =
  "ocamljava_thread_start"

external yield : unit -> unit =
  "ocamljava_thread_yield"
