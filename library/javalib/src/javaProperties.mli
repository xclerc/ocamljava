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

(** Utility functions for Java properties. *)


(** {6 Instance creation} *)

type t = java'util'Properties java_instance
(** The type of properties. *)

val make : ?defaults:t -> unit -> t
(** Returns a new {java java.util.Properties} instance, with the passed
    defaults if not [null], empty otherwise. *)


(** {6 Properties retrieval} *)

val get_property : t -> JavaString.t -> JavaString.t option
(** [get_property props key] returns the value of the property whose key
    is [key] in [props] if found, [None] otherwise; see
    {java java.util.Properties#getProperty(java.lang.String)}. *)
  
val get_property_default : t -> JavaString.t -> JavaString.t -> JavaString.t
(** [get_property_default props key def] returns the value of the
    property whose key is [key] in [props] if found, [def] otherwise; see
    {java java.util.Properties#getProperty(java.lang.String, java.lang.String)}. *)

val string_property_names : t -> JavaString.t list
(** [string_property_names props] returns the list of the keys for the
    properties defined in [props] (including defaults); see
    {java java.util.Properties#stringPropertyNames()}. *)


(** {6 Properties modification} *)

val set_property : t -> JavaString.t -> JavaString.t -> JavaString.t option
(** [set_property props key value] set the property with key [key] to
    value [value] in [props], returning the previous value if any; see
    {java java.util.Properties#setProperty(java.lang.String, java.lang.String)}. *)


(** {6 I/O operations} *)

val load : t -> java'io'InputStream java_extends -> unit
(** [load props str] loads the properties from stream [str] into [props];
    see {java java.util.Properties#load(java.io.InputStream)}.

    @raise Java_exception if an i/o error occurs *)

val load_from_xml : t -> java'io'InputStream java_extends -> unit
(** [load_xml props str] loads the properties from stream [str] into
    [props]; see {java java.util.Properties#loadFromXML(java.io.InputStream)}.

    @raise Java_exception if an i/o error occurs *)

val store : t -> ?comment:JavaString.t -> java'io'OutputStream java_extends -> unit
(** [store props str] stores the properties [props] onto the stream [str]
    with optional comment [commment]; see
    {java java.util.Properties#store(java.io.OutputStream, java.lang.String)}.

    @raise Java_exception if an i/o error occurs *)

val store_to_xml : t -> ?comment:JavaString.t -> java'io'OutputStream java_extends -> unit
(** [store_to_xml props str] stores the properties [props] onto the
    stream [str] with optional comment [commment]; see
    {java java.util.Properties#storeToXML(java.io.OutputStream, java.lang.String)}.

    @raise Java_exception if an i/o error occurs *)


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
