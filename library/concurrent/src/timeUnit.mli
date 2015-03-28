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

(** Time units. *)


type t = java'util'concurrent'TimeUnit java_instance
(** The type of units used to express durations ({i e.g.} for timeouts). *)

val nanoseconds : t
(** Time unit for nanoseconds. *)

val microseconds : t
(** Time unit for microseconds. *)

val milliseconds : t
(** Time unit for milliseconds. *)

val seconds : t
(** Time unit for seconds. *)

val minutes : t
(** Time unit for minutes. *)

val hours : t
(** Time unit for hours. *)

val days : t
(** Time unit for days. *)

val convert : src:t -> dst:t -> java_long -> java_long
(** [convert ~src ~dst x] converts the value [x] from unit [src] to unit
    [dst]; see {java java.util.concurrent.TimeUnit#convert(long,java.util.concurrent.TimeUnit)} *)
