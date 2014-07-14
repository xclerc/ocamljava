(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (asmcomp/asmpackager.mli in the OCaml source
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

(** "Package" a set of .cmj/.jo files into one .cmj/.jo file having the
    original compilation units as sub-modules. *)

val package_files: Format.formatter -> string list -> string -> unit
(** [package_files fmt files targetcmj] packages [files] into
    [targetcmj], [fmt] being used for verbose output. *)

type error =
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error of string
  | File_not_found of string
  | File_compiled_with_a_different_int_size of string

exception Error of error

val report_error: Format.formatter -> error -> unit
(** Pretty-prints an error. *)
