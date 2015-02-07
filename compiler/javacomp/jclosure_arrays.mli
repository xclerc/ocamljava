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

val is_java_array_primitive : string -> bool
(** Test whether the primitive whose identifier is passed is a primitive
    operating on a Java array ([get]/[set]/[length]/[to_object]/[of_object]
    on any Java array type). *)

val java_array_primitive_infos : string -> Jlambda.java_primitive * Lambda.repr
(** Returns the Java primitive and return type representation for the
    primitive whose identifier is passed, raising an exception if such a
    primitive does not exist. *)

val optimize_intarray_call : Jlambda.function_description -> Types.type_expr option list -> Jlambda.function_description
(** [optimize_intarray_call fundesc params] optimizes a call to [fundesc]
    made with parameters [params] by redirecting the call to another
    function specialized for [int] arrays.

    Returns [fundesc] if the call cannot be optimized. *)

val optimize_intarray_primitive : Lambda.primitive -> (Lambda.lambda * Types.type_expr option) list -> Lambda.primitive
(** [optimize_intarray_primitive prim params] optimizes a call to [prim]
    made with parameters [params] by redirecting the call to another
    primitive specialized for [int] arrays].

    Returns [prim] if the call cannot be optimized. *)
