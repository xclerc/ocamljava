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


type t =
  | Nanoseconds (** Associated value is a number of nanoseconds. *)
  | Microseconds (** Associated value is a number of microseconds. *)
  | Milliseconds (** Associated value is a number of milliseconds. *)
  | Seconds (** Associated value is a number of seconds. *)
  | Minutes (** Associated value is a number of minutes. *)
  | Hours (** Associated value is a number of hours. *)
  | Days (** Associated value is a number of days. *)
(** The type of units used to express durations ({i e.g.} for timeouts). *)

external convert : t -> t -> int64 -> int64 =
  "ocamljava_timeunit_convert"
(** [convert f t x] converts the value [x] from unit [f] to unit [t]. *)
