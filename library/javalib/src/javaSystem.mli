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

(** Miscellaneous (properties, time, garbage collection, environment). *)


(** {6 Properties} *)

val clear_property : JavaString.t -> JavaString.t
(** [clear_property name] removes the system property named [name],
    returning its previous value if any; see
    {java java.lang.System#clearProperty(java.lang.String)}.

    @raise Java_exception if [name] is [null]
    @raise Java_exception if [name] is empty
    @raise Java_exception if security manager doesn't allow property
                          modification *)

val get_property : JavaString.t -> JavaString.t
(** [get_property name] returns the value of the system property named
    [name] if defined, [null] otherwise; see
    {java java.lang.System#getProperty(java.lang.String)}.

    @raise Java_exception if [name] is [null]
    @raise Java_exception if [name] is empty
    @raise Java_exception if security manager doesn't allow property
                          lookup *)

val get_property_default : JavaString.t -> JavaString.t -> JavaString.t
(** [get_property_default name default] returns the value of the system
    property named [name] if defined, [default] otherwise; see
    {java java.lang.System#getProperty(java.lang.String, java.lang.String)}.

    @raise Java_exception if [name] is [null]
    @raise Java_exception if [name] is empty
    @raise Java_exception if security manager doesn't allow property
                          lookup *)

val set_property : JavaString.t -> JavaString.t -> JavaString.t
(** [set_property name value] change the system property named [name] to
    [value], returning its previous value if any; see
    {java java.lang.System#setProperty(java.lang.String, java.lang.String)}.

    @raise Java_exception if [name] is [null]
    @raise Java_exception if [name] is empty
    @raise Java_exception if security manager doesn't allow property
                          modification *)


(** {6 Time} *)

val current_time_millis : unit -> java_long
(** Returns the number of milliseconds since January 1, 1970 UTC at
    midnight; see {java java.lang.System#currentTimeMillis()}. *)

val nano_time : unit -> java_long
(** Returns the value of the high-resolution time source in nanoseconds.
    Should be used to measure elapsed time; see
    {java java.lang.System#nanoTime()}. *)


(** {6 Garbage collector} *)

val gc : unit -> unit
(** Runs the garbage collector; see {java java.lang.System#gc()}. *)

val run_finalization : unit -> unit
(** Runs the finalization methods of objects pending finalization; see
    {java java.lang.System#runFinalization()}. *)


(** {6 Environment} *)

val get_env : JavaString.t -> JavaString.t
(** [get_env name] returns the value of the environment variable named
    [name] if defined, [null] otherwise; see
    {java java.lang.System#getenv(java.lang.String)}.

    @raise Java_exception if [name] is [null]
    @raise Java_exception if security manager doesn't allow environment
                          variable lookup *)
