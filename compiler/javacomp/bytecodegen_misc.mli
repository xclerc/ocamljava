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

(** Utilities for bytecode generation. *)

module State : sig
  val reset_state : file:string -> clas:string -> func:string -> unit
  val current_file : unit -> string
  val current_class : unit -> string
  val current_function : unit -> string
  val current_module : unit -> string
  val is_same_function : string -> string -> bool
  val add_exception : BaristaLibrary.Utils.u2 -> BaristaLibrary.Utils.u2 -> BaristaLibrary.Utils.u2 -> unit
  val compile_exception_table : unit -> BaristaLibrary.Attribute.exception_table_element list
  val add_debug_info : int -> Debuginfo.t -> unit
  val compile_line_numbers : unit -> (BaristaLibrary.Utils.u2 * BaristaLibrary.Utils.u2) list
  val add_catch : int -> int ref -> unit
  val get_catch_offset : int -> int ref
  val incr_tail_calls : unit -> unit
  val incr_non_tail_calls : unit -> unit
  val compile_method_infos : unit -> BaristaLibrary.Annotation.t
end

val repeat_parameters : int -> BaristaLibrary.Descriptor.for_field list

val repeat_doubles : int -> BaristaLibrary.Descriptor.for_field list

val kload : Macroinstr.value_kind -> int -> Instrtree.t

val kstore : Macroinstr.value_kind -> int -> Instrtree.t

val compile_conversion : ?relaxed_mode:bool -> Macroinstr.value_kind -> Macroinstr.value_kind -> Instrtree.t

type error =
  | Method_too_long of string * bool * int
  | Unable_to_compute_stack of string * string
  | Unable_to_optimize of string * string
  | Invalid_external_declaration of string
  | Varargs_should_be_literal_array

exception Error of error

val report_error : Format.formatter -> error -> unit

val check_jump : int -> BaristaLibrary.Utils.s2
