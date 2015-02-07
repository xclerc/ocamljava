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

(** Support for [Format] module for Java strings. *)

val fprintf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.fprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val printf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.printf] to print a Java
    string through a ["%a"] conversion specification. *)

val eprintf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.eprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val sprintf : unit -> JavaString.t -> string
(** This function can be passed to [Format.sprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val asprintf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.bprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val ifprintf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.ifprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val kfprintf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.kfprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val ikfprintf : Format.formatter -> JavaString.t -> unit
(** This function can be passed to [Format.ikfprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val ksprintf : unit -> JavaString.t -> string
(** This function can be passed to [Format.ksprintf] to print a Java
    string through a ["%a"] conversion specification. *)
