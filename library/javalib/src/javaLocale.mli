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

(** Utility functions for Java locales. *)


(** {6 Instance creation} *)

type t = java'util'Locale java_instance
(** The type of locales. *)

val make : ?country:JavaString.t -> ?variant:JavaString.t -> language:JavaString.t -> t
(** Returns a new {java java.util.Locale} instance, for the passed
    [language], [country] and [variant]. [variant] is ignored if no
    [country] is passed.

    @raise Java_exception if [language] is [null] *)

val get_available_locales : unit -> t list
(** Returns the list of all available locales. *)

val get_default : unit -> t
(** Returns the default locale. *)

val set_default : t -> java_void
(** [set_default loc] changes the default locale to [loc].

    @raise Java_exception if [loc] is [null] *)


(** {6 Properties} *)

val get_display_name : t -> JavaString.t
(** Returns the name of the passed locale; see
    {java java.util.Locale#getDisplayName()}. *)

val get_language : t -> JavaString.t
(** Returns the language of the passed locale; see
    {java java.util.Locale#getLanguage()}. *)

val get_display_language : t -> JavaString.t
(** Returns the language of the passed locale; see
    {java java.util.Locale#getDisplayLanguage()}. *)

val get_iso3_language : t -> JavaString.t
(** Returns the language of the passed locale; see
    {java java.util.Locale#getISO3Language()}. *)

val get_country : t -> JavaString.t
(** Returns the country of the passed locale; see
    {java java.util.Locale#getCountry()}. *)

val get_display_country : t -> JavaString.t
(** Returns the country of the passed locale; see
    {java java.util.Locale#getDisplayCountry()}. *)

val get_iso3_country : t -> JavaString.t
(** Returns the country of the passed locale; see
    {java java.util.Locale#getISO3Country()}. *)

val get_variant : t -> JavaString.t
(** Returns the variant of the passed locale; see
    {java java.util.Locale#getVariant()}. *)

val get_display_variant : t -> JavaString.t
(** Returns the variant of the passed locale; see
    {java java.util.Locale#getDisplayVariant()}. *)

val get_script : t -> JavaString.t
(** Returns the script of the passed locale; see
    {java java.util.Locale#getScript()}. *)

val get_display_script : t -> JavaString.t
(** Returns the script of the passed locale; see
    {java java.util.Locale#getDisplayScript()}. *)

val to_language_tag : t -> JavaString.t
(** Returns the IETF BCP 47 language tag of the passed locale; see
    {java java.util.Locale#toLanguageTag()}. *)


(** {6 Predefined locales} *)

(** {7 Languages} *)

val chinese             : unit -> t
val english             : unit -> t
val french              : unit -> t
val german              : unit -> t
val italian             : unit -> t
val japanese            : unit -> t
val korean              : unit -> t
val simplified_chinese  : unit -> t
val traditional_chinese : unit -> t

(** {7 countries} *)

val canada        : unit -> t
val canada_french : unit -> t
val china         : unit -> t
val france        : unit -> t
val germany       : unit -> t
val italy         : unit -> t
val japan         : unit -> t
val korea         : unit -> t
val prc           : unit -> t
val taiwan        : unit -> t
val uk            : unit -> t
val us            : unit -> t


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
