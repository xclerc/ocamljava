(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Utility functions for code generation. *)


type error =
  | Command_line_inconsistency of string
  | Cannot_find_cmi_file of string
  | Invalid_cmi_file of string
  | Invalid_cmj_file of string
  | Cannot_determine_function of Ident.t
  | Cannot_translate_open_polymorphic_variant of Ident.t
  | Cannot_translate_polymorphic_variant of Ident.t
  | Cannot_translate_class_type of Ident.t
  | Cannot_map_type of string
  | Cannot_find_type of string
  | Tuple_is_too_large of int
  | Function_arity_is_too_large of int
  | Cannot_inherit
  | Cannot_contain_value
  | Only_asbtract_types_and_functions
  | Cannot_determine_functor_signature

exception Exception of error

val string_of_error : error -> string

val fail : error -> 'a

val map_option : 'a option list -> 'a list

val main_static_block : JavaAST.statement list ref
(** Contents of the static block for the main generated class. *)

val add_static_block : JavaAST.statement list -> unit
(** Adds statements to the static block for the main generated class. *)

val clear_static_block : unit -> unit
(** Empties the static block for the main generated class. *)

val get : JavaAST.expression -> int -> JavaAST.expression
(** [get e i] returns the code equivalent to {i e.get(i)}. *)

val set : JavaAST.expression -> int -> JavaAST.expression -> JavaAST.expression
(** [set e i v] returns the code equivalent to {i e.set(i, v)}. *)

val get_double : JavaAST.expression -> int -> JavaAST.expression
(** [get_double e i] returns the code equivalent to {i e.getDouble(i)}. *)

val set_double : JavaAST.expression -> int -> JavaAST.expression -> JavaAST.expression
(** [set_double e i v] returns the code equivalent to {i e.setDouble(i, v)}. *)

val get_global : int -> JavaAST.expression
(** [get_global i] returns the code equivalent to {i currClass.getGlobal().get(i)}. *)

val create_block : int -> JavaAST.expression list -> JavaAST.expression
(** [create_block tag [v1; ...; vn]] returns the code equivalent to
    {i Value.createBlock(tag, v1, ..., vn)}. *)

val create_double_array : JavaAST.expression list -> JavaAST.expression
(** [create_double_array [v1; ...; vn]] returns the code equivalent to
    {i Value.createDoubleArray(v1, ..., vn)}. *)

val create_long : int -> JavaAST.expression
(** [create_long x] returns the code equivalent to {i Value.createLong(x)}. *)

val make_wrapper_elements : string -> ?suffix:string -> (string * int) list
 -> (JavaAST.modifier list * JavaAST.type_ * string * JavaAST.expression option) list * JavaAST.method_ * JavaAST.method_ * JavaAST.method_ * JavaAST.method_

val make_wrapper_cstr_elements :
    (JavaAST.modifier list * JavaAST.type_ * string * JavaAST.expression option) list ->
      (string * int) list ->
        ((JavaAST.type_ * string) list) * (JavaAST.statement list)

val primitive_type : JavaAST.type_ -> bool
(** Tests whether the passed type designates a primitive Java type. *)

val flatten_arrow : Types.type_expr -> Types.type_expr * (Types.type_expr list)
(** Flattens the passed arrow type (currifying a tuplified function), returning:
    - function return type;
    - function parameter types. *)

val flatten_arrow_not_tuple : Types.type_expr -> Types.type_expr * (Types.type_expr list)
(** Similar to [flatten_arrow] except that function is not currified. *)

val flatten_functor : (Ident.t * Types.signature * (string list)) list -> Types.module_type -> Path.t * ((Ident.t * Path.t) list) * (string * string * string) list

val is_unit : Types.type_expr -> bool
(** Tests whether the passed type is equal to [unit]. *)

val cast_if_needed : JavaAST.type_ -> JavaAST.expression -> JavaAST.expression
(** [cast_if_needed t e] adds a cast from [e] to type [t] if [e] is a
    reference and not already a cast. *)

val not_an_object : Types.type_expr option -> bool
(** [not_an_object x] returns [false] iff [x] is [Some t] where [t] is an
    object type. *)

val make_basic_object_methods : string -> string -> bool -> JavaAST.method_ * JavaAST.method_ * JavaAST.method_
(** Returns basic implementations for the following methods:
    - {i hashCode()};
    - {i equals(Object)};
    - {i toString()}. *)
