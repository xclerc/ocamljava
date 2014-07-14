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

type function_label = {
    fl_class : string;
    fl_method : string;
  }

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
and jlamdba_loop_inlining_info = int option

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

open BaristaLibrary

let convert_class_name cn =
  cn
  |> Name.external_utf8_for_class
  |> UTF8.to_string

let convert_method_name mn =
  mn
  |> Name.utf8_for_method
  |> UTF8.to_string

let convert_field_name fn =
  fn
  |> Name.utf8_for_field
  |> UTF8.to_string

let rec convert_java_type : Descriptor.java_type -> java_type = function
  | #Descriptor.non_void_java_type as jt ->
      (convert_java_type_no_void jt :> java_type)
  | `Void -> `Void
and convert_java_type_no_void : Descriptor.non_void_java_type -> non_void_java_type = function
  | `Boolean -> `Boolean
  | `Byte -> `Byte
  | `Char -> `Char
  | `Double -> `Double
  | `Float -> `Float
  | `Int -> `Int
  | `Long -> `Long
  | `Short -> `Short
  | `Class cn -> `Class (convert_class_name cn)
  | `Array at -> `Array (convert_java_type_no_void at)
and convert_array_type : Descriptor.array_type -> array_type = function
  | `Array at -> `Array (convert_java_type_no_void at)
and convert_java_types : Descriptor.non_void_java_type list -> non_void_java_type list =
  fun l ->
    List.map convert_java_type_no_void l

let unconvert_class_name cn =
  cn
  |> UTF8.of_string
  |> Name.make_for_class_from_external

let unconvert_method_name mn =
  mn
  |> UTF8.of_string
  |> Name.make_for_method

let unconvert_field_name fn =
  fn
  |> UTF8.of_string
  |> Name.make_for_field

let rec unconvert_java_type : java_type -> Descriptor.java_type = function
  | #non_void_java_type as jt ->
      (unconvert_java_type_no_void jt :> Descriptor.java_type)
  | `Void -> `Void
and unconvert_java_type_no_void : non_void_java_type -> Descriptor.non_void_java_type = function
  | `Boolean -> `Boolean
  | `Byte -> `Byte
  | `Char -> `Char
  | `Double -> `Double
  | `Float -> `Float
  | `Int -> `Int
  | `Long -> `Long
  | `Short -> `Short
  | `Class cn -> `Class (unconvert_class_name cn)
  | `Array at -> `Array (unconvert_java_type_no_void at)
and unconvert_array_type : array_type -> Descriptor.array_type = function
  | `Array at -> `Array (unconvert_java_type_no_void at)
and unconvert_java_types : non_void_java_type list -> Descriptor.non_void_java_type list =
  fun l ->
    List.map unconvert_java_type_no_void l

let rec repr_of_java_type = function
  | `Boolean        -> Lambda.LR_bool
  | `Byte           -> Lambda.LR_int
  | `Char           -> Lambda.LR_int
  | `Double         -> Lambda.LR_float
  | `Float          -> Lambda.LR_float
  | `Int            -> Lambda.LR_int32
  | `Long           -> Lambda.LR_int64
  | `Short          -> Lambda.LR_int
  | `Void           -> Lambda.LR_unit
  | `Class cn       -> Lambda.LR_java_instance cn
  | `Array `Boolean -> Lambda.LR_java_boolean_array
  | `Array `Byte    -> Lambda.LR_java_byte_array
  | `Array `Char    -> Lambda.LR_java_char_array
  | `Array `Double  -> Lambda.LR_java_double_array
  | `Array `Float   -> Lambda.LR_java_float_array
  | `Array `Int     -> Lambda.LR_java_int_array
  | `Array `Long    -> Lambda.LR_java_long_array
  | `Array `Short   -> Lambda.LR_java_short_array
  | `Array at       -> Lambda.LR_java_reference_array (repr_of_java_type (at :> java_type))
