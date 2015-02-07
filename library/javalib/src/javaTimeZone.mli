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

(** Utility functions for Java time zones. *)


(** {6 Instance creation} *)

type t = java'util'TimeZone java_instance
(** The type of time zones. *)

val make : JavaString.t -> t
(** Returns a new {java java.util.TimeZone} instance, corresponding to
    the passed identifier. *)
    
val get_available_ids : unit -> JavaString.t list
(** Returns the list of all time zone identifiers. *)

val get_default : unit -> t
(** Returns the default time zone. *)

val set_default : t -> unit
(** [set_default tz] changes the default time zone to [tz] if not [null].
    Otherwise, the default time zone is reset to its original value
    (at VM start). *)


(** {6 Properties} *)

val get_display_name : t -> JavaString.t
(** Returns the name for the passed time zone, as a {i long} string; see
    {java java.util.TimeZone#getDisplayName()}. *)

val get_id : t -> JavaString.t
(** Returns the identifier for the passed time zone; see
    {java java.util.TimeZone#getID()}. *)


(** {6 Null value} *)

val null : t
(** The [null] value. *)

external is_null : t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : t -> t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : t option -> t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
