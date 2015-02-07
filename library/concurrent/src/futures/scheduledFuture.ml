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

type 'a t

external cancel : 'a t -> bool -> bool =
  "ocamljava_future_cancel"

external get : 'a t -> 'a =
  "ocamljava_future_get"

external get_time : 'a t -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_future_get_time"

external is_cancelled : 'a t -> bool =
  "ocamljava_future_is_cancelled"

external is_done : 'a t -> bool =
  "ocamljava_future_is_done"

external get_delay : 'a t -> TimeUnit.t -> int64 =
  "ocamljava_scheduledfuture_get_delay"
