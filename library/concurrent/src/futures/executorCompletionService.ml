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

open Class'java'util'concurrent'Executor
open Class'java'util'concurrent'ExecutorCompletionService
open Class'java'util'concurrent'TimeUnit

type t = _'ExecutorCompletionService java_instance

let make tpe =
  Java.make "ExecutorCompletionService(Executor)" tpe

let poll ecs =
  let res = Java.call "ExecutorCompletionService.poll()" ecs in
  if Java.is_null res then
    None
  else
    Some res

let poll_time ecs time timeunit =
  let res = Java.call "ExecutorCompletionService.poll(long,TimeUnit)" ecs time timeunit in
  if Java.is_null res then
    None
  else
    Some res

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_executorcompletionservice_submit"

let take ecs =
  Java.call "ExecutorCompletionService.take()" ecs


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
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
