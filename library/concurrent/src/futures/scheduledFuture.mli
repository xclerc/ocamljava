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

(** Computations run in background with a delay. *)


type 'a t = java'util'concurrent'ScheduledFuture java_instance
(** The type of futures, that are computations run in background with a
    delay. *)

val cancel : 'a t -> bool -> bool
(** Same as {!Future.cancel}. *)

val get : 'a t -> 'a
(** Same as {!Future.get}. *)

val get_time : 'a t -> java_long -> TimeUnit.t -> 'a
(** Same as {!Future.get_time}. *)

val is_cancelled : 'a t -> bool
(** Same as {!Future.is_cancelled}. *)

val is_done : 'a t -> bool
(** Same as {!Future.is_done}. *)

val get_delay : 'a t -> TimeUnit.t -> java_long
(** Returns the remaining delay for the computation, in the passed time
    unit. *)
