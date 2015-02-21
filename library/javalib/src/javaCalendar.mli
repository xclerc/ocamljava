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

(** Utility functions for Java calendars. *)


(** {6 Instance creation} *)

type t = java'util'Calendar java_instance
(** The type of calendars. *)

val now : ?locale:JavaLocale.t -> ?zone:JavaTimeZone.t -> unit -> t
(** Returns a new {java java.util.Calendar} instance for the current time,
    using the passed local and/or time zone. *)


(** {6 Fields} *)

type am_pm =
  | AM (** The hour is between midnight (included) and noon (excluded). *)
  | PM (** The hour is between noon (included) and midnight (excluded). *)
(** Whether the hour is before or after noon. *)

type month =
  | January    (** Constant for the {i month} field. *)
  | February   (** Constant for the {i month} field. *)
  | March      (** Constant for the {i month} field. *)
  | April      (** Constant for the {i month} field. *)
  | May        (** Constant for the {i month} field. *)
  | June       (** Constant for the {i month} field. *)
  | July       (** Constant for the {i month} field. *)
  | August     (** Constant for the {i month} field. *)
  | September  (** Constant for the {i month} field. *)
  | October    (** Constant for the {i month} field. *)
  | November   (** Constant for the {i month} field. *)
  | December   (** Constant for the {i month} field. *)
  | Undecimber (** A special thirteen month (for non-gregorian calendars). *)
(** The different month values. *)

type day =
  | Monday    (** Constant for the {i day} field. *)
  | Tuesday   (** Constant for the {i day} field. *)
  | Wednesday (** Constant for the {i day} field. *)
  | Thursday  (** Constant for the {i day} field. *)
  | Friday    (** Constant for the {i day} field. *)
  | Saturday  (** Constant for the {i day} field. *)
  | Sunday    (** Constant for the {i day} field. *)
(** The different day values. *)

type _ field =
  | Era                  : java_int field (** BC/AD in gregorian calendar. *)
  | Year                 : java_int field (** Year (including century). *)
  | Month                : month    field (** Month. *)
  | Week_of_month        : java_int field (** Range: 0..6 (1 is the first full week). *)
  | Week_of_year         : java_int field (** Range: 1..53. *)
  | Date                 : java_int field (** Synonym for [Day_of_month]. *)
  | Day_of_month         : java_int field (** Starts at 1. *)
  | Day_of_week          : day      field (** Day. *)
  | Day_of_week_in_month : java_int field (** Starts at 1. *)
  | Day_of_year          : java_int field (** Starts at 1. *)
  | AM_PM                : am_pm    field (** Whether AM or PM. *)
  | Hour                 : java_int field (** Range: 0..11. *)
  | Hour_of_day          : java_int field (** Range: 0..23. *)
  | Minute               : java_int field (** Range: 0..59. *)
  | Second               : java_int field (** Range: 0..59. *)
  | Millisecond          : java_int field (** Range: 0..999. *)
  | Dst_offset           : java_int field (** Raw offset of daylight saving in milliseconds. *)
  | Zone_offset          : java_int field (** Raw offset from GMT in milliseconds. *)
(** The different fields of a calendar. *)


(** {6 Properties} *)

val get_time : t -> java_long
(** [get_time cal] returns the time of the passed calendar, as the number
    of milliseconds since {i 1970-01-01 00:00:00}; see
    {java java.util.Calendar#getTimeInMillis()}. *)

val after : t -> t -> bool
(** [after cal when] returns [true] iff [cal] is strictly later than
    [when]; see {java java.util.Calendar#after(java.lang.Object)}. *)

val before : t -> t -> bool
(** [before cal when] returns [true] iff [cal] is strictly later than
    [when]; see {java java.util.Calendar#before(java.lang.Object)}. *)

val compare_to : t -> t -> int
(** Compares the passed calendars; see
    {java java.util.Calendar#compareTo(java.util.Calendar)}. *)

val get : t -> 'a field -> 'a
(** [get cal f] returns the value for field [f] in calendar [cal]; see
    {java java.util.Calendar#get(int)}. *)

val get_time_zone : t -> JavaTimeZone.t
(** Returns the time zone for the passed calendar; see
    {java java.util.Calendar#getTimeZone()}. *)

(** {6 Creation of new calendar values} *)

val add : t -> 'a field -> java_int -> t
(** [add cal f delta] returns a new instance that is equal to [cal], with
    the field [f] modified by adding [delta]; see
    {java java.util.Calendar#add(int, int)}. *)

val set : t -> 'a field -> 'a -> t
(** [set cal f v] returns a new instance that is equal to [cal], with
    the field [f] changed to [v]; see
    {java java.util.Calendar#set(int, int)}. *)

val clear : t -> 'a field -> t
(** [clear cal f] returns a new instance that is equal to [cal], with
    the field [f] cleared; see
    {java java.util.Calendar#clear(int)}. *)

val clear_all : t -> t
(** [clear_all cal] returns a new instance that is equal to [cal], with
    the all fields cleared; see
    {java java.util.Calendar#clear()}. *)

val set_time_zone : t -> JavaTimeZone.t -> t
(** [set_time_zone cal tz] returns a new instance that is equal to [cal],
    with the time zone set to [tz]; see
    {java java.util.Calendar#setTimeZone(java.util.TimeZone)}. *)


(** {6 Conversions} *)

val to_date : t -> JavaDate.t
(** Converts the passed calendar into a date. *)

val of_date : JavaDate.t -> t
(** Converts the passed date into a calendar. *)


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
