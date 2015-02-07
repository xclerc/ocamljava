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

(** Support for [Printf] module for Java strings. *)

val fprintf : out_channel -> JavaString.t -> unit
(** This function can be passed to [Printf.fprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val printf : out_channel -> JavaString.t -> unit
(** This function can be passed to [Printf.printf] to print a Java
    string through a ["%a"] conversion specification. *)

val eprintf : out_channel -> JavaString.t -> unit
(** This function can be passed to [Printf.eprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val sprintf : unit -> JavaString.t -> string
(** This function can be passed to [Printf.sprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val bprintf : Buffer.t -> JavaString.t -> unit
(** This function can be passed to [Printf.bprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val ifprintf : 'a -> 'b -> unit
(** This function can be passed to [Printf.ifprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val kfprintf : out_channel -> JavaString.t -> unit
(** This function can be passed to [Printf.kfprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val ikfprintf : out_channel -> JavaString.t -> unit
(** This function can be passed to [Printf.ikfprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val ksprintf : unit -> JavaString.t -> string
(** This function can be passed to [Printf.ksprintf] to print a Java
    string through a ["%a"] conversion specification. *)

val kbprintf : Buffer.t -> JavaString.t -> unit
(** This function can be passed to [Printf.kbprintf] to print a Java
    string through a ["%a"] conversion specification. *)
