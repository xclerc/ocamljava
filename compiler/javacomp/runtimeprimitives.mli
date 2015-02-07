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

(** Definition of OCaml-Java primitives. *)

type error =
  | Unavailable_primitive_definitions
  | Invalid_primitive_definitions
  | Undefined_class of string * (string option)
  | Undefined_primitive of string

exception Error of error
(** Exception raised when a primitive or class cannot be found. *)


type description = {
    primdesc_class : string;
    primdesc_method : string;
    primdesc_parameters : Lambda.repr list;
    primdesc_return : Lambda.repr;
    primdesc_javadesc : BaristaLibrary.Descriptor.for_method;
  }

val init : unit -> unit
(** Initializes the module by loading primitive definitions from the
    ["ocamlrun.primitives"] file unless [Jclflags.nobuiltin] is [true].

    Also loads primitives from [Jclflags.additional_providers] classes
    from the classpath.

    Primitive definitions are printed on standard output if
    [Jclflags.dump_primitives] is [true].

    This function is automatically called at the first call to
    [get_description] if it has not already been called.

    Raises [Error] if initialization fails. *)

val get_description : string -> description
(** Returns the description for the primitive whose name is passed.

    First looks for a primitive with a trailing ["$"] appended to the
    passed name (designating the unboxed implementation of the
    primitive), then looks exactly for the passed name.

    Raises [Error] if such a class does not exist or if initialization
    fails. *)

val report_error : Format.formatter -> error -> unit
(** Pretty-prints an error. *)
