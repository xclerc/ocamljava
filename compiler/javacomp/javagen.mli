(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/cmmgen.mli in the OCaml source
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

(** Compilation from lambda to Java bytecode. *)

val compile_implementation : string -> Format.formatter -> int * Lambda.lambda -> unit
(** [compile_implementation prefixname ppf (size, lam)] compiles lambda
    [lam], [size] being the size of its associated global. [prefixname]
    is the prefix path of created files, while [fmt] is used for
    pretty-printing. *)

type error =
    Invalid_servlet_signature of string

exception Error of error

val report_error: Format.formatter -> error -> unit
(** Pretty-prints an error. *)
