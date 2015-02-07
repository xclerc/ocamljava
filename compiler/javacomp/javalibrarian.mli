(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/asmlibrarian.mli in the OCaml source
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

(** Build libraries of .cmj files. *)

val create_archive: string list -> string -> unit
(** [create_archive file_list lib_name] compiles a library named
    [lib_name] from files in [file_list] (that should be .cmj files). *)

type error =
    File_not_found of string
  | Archiver_error of string * string
  | File_compiled_with_a_different_int_size of string

exception Error of error

val report_error: Format.formatter -> error -> unit
(** Pretty-prints an error. *)
