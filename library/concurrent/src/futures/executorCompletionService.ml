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

external make : ThreadPoolExecutor.t -> t =
  "ocamljava_executorcompletionservice_make"

external poll : t -> 'a Future.t option =
  "ocamljava_executorcompletionservice_poll"

external poll_time : t -> int64 -> TimeUnit.t -> 'a Future.t option =
  "ocamljava_executorcompletionservice_poll_time"

external submit : t -> ('a -> 'b) -> 'a -> 'b Future.t =
  "ocamljava_executorcompletionservice_submit"

external take : t -> 'a Future.t =
  "ocamljava_executorcompletionservice_take"
