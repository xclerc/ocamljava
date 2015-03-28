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

open Class'java'util'Date
open Class'java'util'concurrent'TimeUnit
open Class'java'util'concurrent'locks'Condition

type t = _'Condition java_instance

let await cond =
  Java.call "Condition.await()" cond

let await_time cond time timeunit =
  Java.call "Condition.await(long,TimeUnit)" cond time timeunit

let await_nanos cond nanos =
  Java.call "Condition.awaitNanos(long)" cond nanos

let await_uninterruptibly cond =
  Java.call "Condition.awaitUninterruptibly()" cond

let await_until cond date =
  Java.call "Condition.awaitUntil(Date)" cond date

let signal cond =
  Java.call "Condition.signal()" cond

let signal_all cond =
  Java.call "Condition.signalAll()" cond
