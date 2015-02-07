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

type t

external make : int32 -> bool -> t =
  "ocamljava_semaphore_make"

external acquire : t -> int32 -> unit =
  "ocamljava_semaphore_acquire"

external acquire_uninterruptibly : t -> int32 -> unit =
  "ocamljava_semaphore_acquire_uninterruptibly"

external available_permits : t -> int32 =
  "ocamljava_semaphore_available_permits"

external drain_permits : t -> int32 =
  "ocamljava_semaphore_drain_permits"

external get_queue_length : t -> int32 =
  "ocamljava_semaphore_get_queue_length"

external has_queued_threads : t -> bool =
  "ocamljava_semaphore_has_queued_threads"

external is_fair : t -> bool =
  "ocamljava_semaphore_is_fair"

external release : t -> int32 -> unit =
  "ocamljava_semaphore_release"

external try_acquire : t -> int32 -> bool =
  "ocamljava_semaphore_try_acquire"

external try_acquire_time : t -> int32 -> int64 -> TimeUnit.t -> bool =
  "ocamljava_semaphore_try_acquire"
