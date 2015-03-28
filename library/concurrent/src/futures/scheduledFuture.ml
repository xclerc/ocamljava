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

open Class'java'util'concurrent'ScheduledFuture
open Class'java'util'concurrent'TimeUnit

type 'a t = _'ScheduledFuture java_instance

let cancel f i =
  Java.call "ScheduledFuture.cancel(boolean)" f i

external get : 'a t -> 'a =
  "ocamljava_future_get"

external get_time : 'a t -> java_long -> TimeUnit.t -> 'a =
  "ocamljava_future_get_time"

let is_cancelled f =
  Java.call "ScheduledFuture.isCancelled()" f

let is_done f =
  Java.call "ScheduledFuture.isDone()" f

let get_delay f timeunit =
  Java.call "ScheduledFuture.getDelay(TimeUnit)" f timeunit
