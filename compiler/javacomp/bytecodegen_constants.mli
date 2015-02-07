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

(** Compilation of atomic and structured constants. *)


val const_class_of_curr_class : string -> BaristaLibrary.Name.for_class
(** Returns the name of the class holding constants for the module whose
    class name is passed. *)

val reset : unit -> unit
(** Resets the sets of atomic and structured constants. *)

val push_int : int64 -> Instrtree.t
(** Return the instructions pushing the passed constant as a boxed value. *)

val push_int32 : int32 -> Instrtree.t
(** Return the instructions pushing the passed constant as a boxed value. *)

val push_int64 : int64 -> Instrtree.t
(** Return the instructions pushing the passed constant as a boxed value. *)

val push_nativeint : nativeint -> Instrtree.t
(** Return the instructions pushing the passed constant as a boxed value. *)

val push_float : float -> Instrtree.t
(** Return the instructions pushing the passed constant as a boxed value. *)

val push_structured_constant : Lambda.structured_constant -> Instrtree.t
(** Return the instructions pushing the passed constant as a boxed value. *)

val get_fields_and_inits : unit -> BaristaLibrary.Field.t list * BaristaLibrary.Instruction.t list
(** Returns the fields used to store boxed constants, and the
    instructions initializing these fields. *)

val compile_class : bool -> (string * BaristaLibrary.Bytes.t) list
(** Compiles the class holding the constants for the module, returning a
    list of (entry path, entry contents) couples. The parameter indicates
    whether marshalled data for constants is needed. *)

val init_class_fields_from_code : unit -> Instrtree.t
(** Returns the instructions initializing fields. *)

val init_class_fields_from_load : unit -> Instrtree.t
(** Returns the instructions initializing fields from a marshalled
    entry. *)
