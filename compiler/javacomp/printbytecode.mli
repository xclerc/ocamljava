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

(** Pretty-printing for bytecode. *)

val bytecode : Format.formatter -> string -> BaristaLibrary.Instruction.t list -> BaristaLibrary.Attribute.exception_table_element list -> unit
(** [bytecode fmt header l e] prints the string [header], then the list
    [l] of bytecode instructions, and finally the exception handlers [e]
    onto [fmt]. *)
