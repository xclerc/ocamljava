(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/asmlink.mli in the OCaml source
 * distribution) is Copyright (C) INRIA.
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

(** Link a set of .cmj/.jo files and produce an executable or a plugin. *)

val link: Format.formatter -> string list -> string -> unit
(** [link objfiles output_name] constructs the executable [output_name]
    from [objfiles]. *)

val link_shared: Format.formatter -> string list -> string -> unit
(** [link_shared objfiles output_name] constructs the plugin [output_name]
    from [objfiles]. *)

val check_consistency: string -> Cmj_format.unit_infos -> Digest.t -> unit
(** [check_consistency file_name unit crc] checks the consistency of
    [file_name] declaring [unit] and whose digest is [crc]. *)

val extract_crc_interfaces: unit -> (string * Digest.t) list
(** Returns the list of unitname/digest couples corresponding to the unit
    interface whose consistency has been checked. *)

val extract_crc_implementations: unit -> (string * Digest.t) list
(** Returns the list of unitname/digest couples corresponding to the unit
    implementation whose consistency has been checked. *)

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Linking_error of string
  | Multiple_definition of string * string * string
  | Missing_cmj of string * string
  | Unable_to_add_class of string
  | Unable_to_add_file of string
  | Duplicate_entry of string
  | Invalid_applet_signature of string
  | File_compiled_with_a_different_int_size of string
  | Invalid_runtime_parameter of string

exception Error of error

val report_error: Format.formatter -> error -> unit
(** Pretty-prints an error. *)
