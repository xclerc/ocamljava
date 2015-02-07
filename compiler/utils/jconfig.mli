(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java compiler is free software; you can redistribute it and/or modify
 * it under the terms of the Q Public License as published by
 * Trolltech (with a change to choice of law).
 *
 * OCaml-Java compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Q Public License for more details.
 *
 * You should have received a copy of the Q Public License
 * along with this program.  If not, see
 * <http://opensource.org/licenses/QPL-1.0>.
 *)

(** OCaml-Java-specific configuration elements. *)

val version : string
(** The OCaml-Java version, not linked to the OCaml version. *)

val ints_are_63_bit_long : bool
(** Values of the [int] type use 63 bits if [true], and use 64 bits
    otherwise. *)

val model : string
(** The destination "architecture". *)

val cmj_magic_number : string
(** The magic number for unit descriptions. *)

val cmja_magic_number : string
(** The magic number for archives of unit descriptions. *)

val cmjs_magic_number : string
(** The magic number for plugins. *)

val ext_compiled : string
(** The file extension for unit descriptions (with leading dot). *)

val ext_library : string
(** The file extension for archives of unit descriptions (with leading dot). *)

val ext_class : string
(** The file extension for Java classes (with leading dot). *)

val ext_consts : string
(** The file extension for marshalled constants (with leading dot). *)

val ext_obj : string
(** The file extension for Java objects (with leading dot). *)

val ext_lib : string
(** The file extension for Java libraries (with leading dot). *)

val ext_exec : string
(** The file extension for Java executables (with leading dot). *)

val ext_plugin : string
(** The file extension for plugins (with leading dot). *)

val default_executable_name : string
(** The name of the produced executable jar file if none is specified. *)

val primitive_definitions : string
(** The base name of the file containing the primitive definitions. *)

val parameters_entry : string
(** The base name of the file containing the runtime parameters. *)

val runtime_support_jar : string
(** The base name of the file containing the runtime support. *)

val main_class : string
(** The simple name of the main class of a regular application. *)

val main_applet_class : string
(** The simple name of the main class of an applet. *)

val main_servlet_class : string
(** The simple name of the main class of a servlet. *)

val get_runtime_jar : unit -> string
(** Returns the absolute path of the runtime jar. *)

val print_config : out_channel -> unit
(** Prints the configuration elements onto the passed output channel. *)
