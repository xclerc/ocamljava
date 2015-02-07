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

(** Compilation from macroinstructions to Java bytecode. *)

val compile_fields : Format.formatter -> BaristaLibrary.Method.t * (BaristaLibrary.Field.t list)
(** Returns the fields for the current class, as a list of fields along
    with the static block initializing the fields. These fields contain
    the method handles for the functions. *)

val compile_function : Format.formatter -> Macroinstr.fundecl -> BaristaLibrary.Method.t
(** Translates the passed macroinstruction-level function into its
    Java-bytecode equivalent. *)

val compile_surrogate : Macroinstr.fundecl -> BaristaLibrary.Method.t
(** Compiles the surrogate (i.e. unboxing parameters, and boxing result
    value) method for the passed function. *)

val compile_class : BaristaLibrary.Field.t list -> BaristaLibrary.Method.t list -> BaristaLibrary.Annotation.t list -> BaristaLibrary.Bytes.t
(** Builds a class from environment and passed lists of fields and
    methods. Returns the compiled class as an array of bytes. *)

val compile_constants_to_method : Format.formatter -> BaristaLibrary.Method.t * bool
(** Compiles the constants, returning the constants as a method. *)

val compile_get_global_method : string -> BaristaLibrary.Method.t
(** Returns a method returning the global for the module. *)
