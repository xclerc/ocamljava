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

open Class'java'util'concurrent'RejectedExecutionHandler

type t = _'RejectedExecutionHandler java_instance

let abort_policy =
  Java.make "java.util.concurrent.ThreadPoolExecutor.AbortPolicy()" ()
  |> Java.cast "RejectedExecutionHandler"

let caller_runs_policy =
  Java.make "java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy()" ()
  |> Java.cast "RejectedExecutionHandler"

let discard_oldest_policy =
  Java.make "java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy()" ()
  |> Java.cast "RejectedExecutionHandler"

let discard_policy =
  Java.make "java.util.concurrent.ThreadPoolExecutor.DiscardPolicy()" ()
  |> Java.cast "RejectedExecutionHandler"
