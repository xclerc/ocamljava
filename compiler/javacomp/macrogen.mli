(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Choice of runtime representation for values, mapping from idents to
    locals, recording of global uses. *)

type error =
  | Special_primitive_string of string

exception Error of error

type add_function = string -> string -> (Ident.t * Lambda.repr) list -> Lambda.repr -> Jlambda.jlambda -> unit
(** Type of callbacks adding a function to the list of functions to
    compile.

    Parameters are:
    - class name;
    - function/method name;
    - parameter identifiers and representations;
    - return representation;
    - function body. *)

val translate : add_function -> bool -> (Ident.t * Lambda.repr) list -> Lambda.repr -> Jlambda.jlambda -> Macroinstr.expression
(** [translate af entry params body] converts the function whose body
    [body], and parameters [params] are passed into a macroinstr. [entry]
    indicates whether the function is the entry function of a module. The
    callback [af] is used to request the compilation of another function. *)

val report_error : Format.formatter -> error -> unit
(** Pretty-prints an error. *)
