(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Compilation of ocamljava-specific primitives. *)

val compile_java_primitive : int -> Jlambda.java_primitive -> Macroinstr.expression list ->
  (int -> Macroinstr.expression list -> Instrtree.t) ->
  (bool -> int -> Macroinstr.expression -> Instrtree.t) ->
  Instrtree.t
(** [compile_java_primitive ofs prim args cl ce] compiles the primitive
    [prim] at offset [ofs] with arguments [args]; the functions [cl] and
    [ce] are used to compile respectively expression lists and simple
    expressions. The first function takes the following parameters:
    - offset;
    - expression list to compile.

    The second function takes the following parameters:
    - whether the expression is in a tail position;
    - offest;
    - expression to compile. *)
