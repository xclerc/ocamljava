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

(** Utility functions for Java dates. *)


(** {6 Instance creation} *)

type t = java'util'Date java_instance
(** The type of dates. *)

val now : unit -> t
(** Returns a new {java java.util.Date} instance for the current time. *)

val make : java_long -> t
(** Returns a new {java java.util.Date} instance for the passed time,
    as the number of milliseconds since {i 1970-01-01 00:00:00}. *)


(** {6 Properties} *)

val get_time : t -> java_long
(** [get_time date] returns the time of the passed date, as the number of
    milliseconds since {i 1970-01-01 00:00:00}; see
    {java java.util.Date#getTime()}. *)

val after : t -> t -> bool
(** [after date when] returns [true] iff [date] is strictly later than
    [when]; see {java java.util.Date#after(java.util.Date)}. *)

val before : t -> t -> bool
(** [before date when] returns [true] iff [date] is strictly earlier than
    [when]; see {java java.util.Date#before(java.util.Date)}. *)

val compare_to : t -> t -> int
(** Compares the passed dates; see
    {java java.util.Date#compareTo(java.util.Date)}. *)


(** {6 Conversions} *)

type format = java'text'DateFormat java_instance
(** The type of date formats. *)

type format_style =
  | Full   (** Format containing all available information. *)
  | Long   (** Usual format - {i e.g.} January 2, 2003. *)
  | Medium (** Abbreviated format - {i e.g.} Jan 2, 2003. *)
  | Short  (** Purely numeric format - {i e.g.} 01-02-03. *)
(** The type of format styles. *)

val make_date_format : ?date_style:format_style -> ?locale:JavaLocale.t -> ?time_zone:JavaTimeZone.t -> unit -> format
(** Creates a date format from the passed style, locale, and time
    zone. *)

val make_date_time_format : ?date_style:format_style -> ?time_style:format_style -> ?locale:JavaLocale.t -> ?time_zone:JavaTimeZone.t -> unit -> format
(** Creates a date/time format from the passed styles, locale, and time
    zone. *)

val make_simple_format : ?pattern:JavaString.t -> ?locale:JavaLocale.t -> ?time_zone:JavaTimeZone.t -> unit -> format
(** Creates a format from the passed pattern, locale, and time zone. The
    locale is actually used iff the pattern is not [null]. The pattern
    format is described in the javadoc for
    {java java.text.SimpleDateFormat}.

    @raise Java_exception if the pattern is invalid *)

val to_string : format -> t -> JavaString.t
(** Converts the passed date into a string. *)

val of_string : format -> JavaString.t -> t
(** Converts the passed string into a date.

    @raise Java_exception if the string does not conform to the format *)


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
