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

(** Signatures of primitives. *)


val is_unary_math_primitive : string -> bool
(** Tests whether the passed external name designates a unary math
    function. *)

val unary_method : string -> string
(** Returns the Java method (in {i java.lang.Math} class) for the passed
    external name.

    Raises [Not_found] if the passed name does not designate a unary math
    function. *)

val is_binary_math_primitive : string -> bool
(** Tests whether the passed external name designates a binary math
    function. *)

val binary_method : string -> string
(** Returns the Java method (in {i java.lang.Math} class) for the passed
    external name.

    Raises [Not_found] if the passed name does not designate a binary
    math function. *)

val signature_of_primitive : Lambda.primitive -> Jlambda.jlambda list -> Macroinstr.parameters * Macroinstr.value_kind
(** Returns the signature of a regular primitive. *)

val signature_of_java_primitive : Jlambda.java_primitive -> Macroinstr.value_kind list * Macroinstr.value_kind
(** Returns the signature of an ocamljava-specific primitive. *)

val simplif_primitive : Lambda.primitive -> string option
(** Simplifies the passed primitive into a call to an external,
    returning [None] if primitive is not optimized. *)
