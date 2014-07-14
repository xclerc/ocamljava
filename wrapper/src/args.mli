(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Handling of command-line parameters. *)


type library_init =
  | Explicit (** Library must be explicitly initialized by user. *)
  | Static (** Library is automatically initialized by a static block. *)
(** Initialization mode of OCaml libraries. *)

type string_mapping =
  | Java_string (** Map OCaml strings to Java strings. *)
  | OCamlString (** Map OCaml strings to mutable strings. *)
  | Byte_array (** Map OCaml strings to byte arrays *)
(** Mapping mode of OCaml strings. *)

type file = {
    file_name : string;
    file_package : string option;
  }
(** Information about files to process. *)

val class_name_prefix : string ref
(** The name prefix to generated classes. *)

val class_name_suffix : string ref
(** The name suffix to generated classes. *)

val includes : string list ref
(** The search path. *)

val library_args : string option ref
(** The arguments to be passed at library initialization. *)

val library_init : library_init ref
(** The initialization mode. *)

val library_package : string option ref
(** The package of the library to be initialized. *)

val no_warnings : bool ref
(** Whether to disable warnings. *)

val package : string option ref
(** The package for generated classes. *)

val string_mapping : string_mapping ref
(** The mapping mode of OCaml strings. *)

val verbose : bool ref
(** Whether to enable verbose mode. *)

val files : file list ref
(** The files provided on the command line. *)

val parse : unit -> unit
(** Parses the command line, and initializes the values exported by this
    module. *)

val class_of_module : string -> string
(** Maps the passed module name to the corresponding class name. *)
