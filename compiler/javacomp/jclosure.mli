(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (asmcomp/closure.mli in the OCaml source
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

(** Introduction of closures, uncurrying, recognition of direct calls. *)

val occurs_var : Ident.t -> Jlambda.jlambda -> bool
(** Tests whether the passed variable occurs in the passed lambda. *)

val is_pure_jlambda : Jlambda.jlambda -> bool
(** Checks whether the passed jlambda is pure ({i i.e.} with no
    side-efect, and without function definition). *)

val intro: int -> Lambda.lambda -> Jlambda.jlambda
(** Converts the passed lambda into its equivalent jlambda, passed [int]
    being the size of global. *)

type error =
    Special_primitive_first_argument

exception Error of error

val report_error: Format.formatter -> error -> unit
(** Pretty-prints an error. *)
