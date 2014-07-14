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

(** Java variant of the lambda code. *)

type function_label = {
    fl_class : string; (** Class name. *)
    fl_method : string; (** Method name. *)
  }
(** Java name for function. *)

type java_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Void
  | `Class of string
  | `Array of 'a ] constraint 'a = non_void_java_type
(** Akin to [BaristaLibrary.Descriptor.java_type], but with class names
    represented by bare strings (for marshalling in cmj files). *)
and non_void_java_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Class of string
  | `Array of 'a ] constraint 'a = non_void_java_type
(** Akin to [BaristaLibrary.Descriptor.non_void_java_type], but with
    class names represented by bare strings (for marshalling in cmj
    files). *)

type array_type =
  [ `Array of 'a ] constraint 'a = [ `Boolean
                                   | `Byte
                                   | `Char
                                   | `Double
                                   | `Float
                                   | `Int
                                   | `Long
                                   | `Short
                                   | `Class of string
                                   | `Array of 'a ]
(** Akin to [BaristaLibrary.Descriptor.array_type], but with class names
    represented by bare strings (for marshalling in cmj files). *)

type java_primitive =
  | Java_constructor of string * non_void_java_type list * bool
  | Java_array of array_type * java_primitive_array_dims
  | Java_array_get of array_type
  | Java_array_set of array_type
  | Java_array_length of array_type
  | Java_array_to_object of array_type
  | Java_array_of_object of array_type
  | Java_method of string * string * java_primitive_call_kind * (non_void_java_type list) * bool * java_type
  | Java_get of string * string * java_primitive_field_kind * non_void_java_type
  | Java_set of string * string * java_primitive_field_kind * non_void_java_type
  | Java_get_null
  | Java_is_null
  | Java_is_not_null
  | Java_equal
  | Java_not_equal
  | Java_instanceof of non_void_java_type
  | Java_cast of non_void_java_type
  | Java_class of java_type
  | Java_throw
  | Java_synchronized of java_primitive_sync_kind * int
  | Java_proxy of java_primitive_proxy
and java_primitive_array_dims = {
    jpad_total : int;
    jpad_init : int;
  }
and java_primitive_call_kind =
  | Static_call
  | Interface_call
  | Virtual_call
and java_primitive_field_kind =
  | Static_field
  | Instance_field
and java_primitive_sync_kind =
  | Inlined_sync
  | Function_sync
and java_primitive_proxy = {
    jpp_interface : string;
    jpp_interfaces : string list;
    jpp_mapping : java_primitive_proxy_binding list;
  }
and java_primitive_proxy_binding = {
    jppb_interface : string;
    jppb_method : string;
    jppb_parameters : non_void_java_type list;
    jppb_ocaml_name : string;
  }

type const =
  | Lambda_const of Lambda.structured_constant
  | Const_targetint of Targetint.t
  | Const_null of non_void_java_type option
  | Const_javastring of string

type jlambda =
  | Jvar of Ident.t
  | Jconst of const
  | Jdirect_apply of function_label * jlambda list * Lambda.repr list * Lambda.repr * Debuginfo.t
  | Jgeneric_apply of jlambda * jlambda list * Debuginfo.t
  | Jclosure of jfunction list * jlambda list
  | Joffset of jlambda * int
  | Jlet of Ident.t * jlambda * jlambda
  | Jletrec of (Ident.t * jlambda) list * jlambda
  | Jprim of Lambda.primitive * jlambda list * Debuginfo.t
  | Jjavaprim of java_primitive * jlambda list * Debuginfo.t
  | Jswitch of jlambda * jlambda_switch
  | Jstaticfail of int * jlambda list
  | Jstaticcatch of int * Ident.t list * jlambda * jlambda
  | Jtrywith of jlambda * Ident.t * jlambda
  | Jifthenelse of jlambda * jlambda * jlambda
  | Jsequence of jlambda * jlambda
  | Jwhile of jlambda * jlambda * jlamdba_loop_inlining_info
  | Jfor of Ident.t * jlambda * jlambda * Asttypes.direction_flag * jlambda * jlamdba_loop_inlining_info
  | Jassign of Ident.t * jlambda
  | Jsend of Lambda.meth_kind * jlambda * jlambda * jlambda list * Debuginfo.t
  | Jnop
and jfunction = {
    label : function_label;
    arity : int;
    params : (Ident.t * Lambda.repr) list;
    return : Lambda.repr;
    body : jlambda;
    dbg : Debuginfo.t;
  }
and jlambda_switch =
    { js_index_consts : int array;
      js_index_blocks : int array;
      js_actions : jlambda array; }
and jlamdba_loop_inlining_info = int option (* number of times the loop is unrolled *)

type function_description =
    { fun_label : function_label;
      fun_arity : int;
      fun_repr_parameters : Lambda.repr list;
      fun_repr_return : Lambda.repr;
      mutable fun_closed : bool;
      mutable fun_inline : (Ident.t list * jlambda) option; }

type value_approximation =
  | Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown of Lambda.repr option
  | Value_integer of Targetint.t
  | Value_integer32 of int32
  | Value_integer64 of int64
  | Value_integernat of nativeint
  | Value_constptr of int
  | Value_float of float
  | Value_java_null of non_void_java_type option
  | Value_java_string of string

val convert_class_name : BaristaLibrary.Name.for_class -> string
val convert_method_name : BaristaLibrary.Name.for_method -> string
val convert_field_name : BaristaLibrary.Name.for_field -> string

val convert_java_type : BaristaLibrary.Descriptor.java_type -> java_type
val convert_java_type_no_void : BaristaLibrary.Descriptor.non_void_java_type -> non_void_java_type
val convert_array_type : BaristaLibrary.Descriptor.array_type -> array_type
val convert_java_types : BaristaLibrary.Descriptor.non_void_java_type list -> non_void_java_type list

val unconvert_class_name : string -> BaristaLibrary.Name.for_class
val unconvert_method_name : string -> BaristaLibrary.Name.for_method
val unconvert_field_name : string -> BaristaLibrary.Name.for_field

val unconvert_java_type : java_type -> BaristaLibrary.Descriptor.java_type
val unconvert_java_type_no_void : non_void_java_type -> BaristaLibrary.Descriptor.non_void_java_type
val unconvert_array_type : array_type -> BaristaLibrary.Descriptor.array_type
val unconvert_java_types : non_void_java_type list -> BaristaLibrary.Descriptor.non_void_java_type list

val repr_of_java_type : java_type -> Lambda.repr
