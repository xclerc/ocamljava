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

open Lambda
open Jlambda

type value_kind =
  | Boxed_value
  | Tagged_int
  | Normalized_int
  | Unboxed_int
  | Unboxed_int32
  | Unboxed_int64
  | Unboxed_nativeint
  | Unboxed_float
  | Unboxed_instance of string
  | Unboxed_java_array of Jlambda.array_type

let int_kind =
  if Jconfig.ints_are_63_bit_long then
    Tagged_int
  else
    Unboxed_int

let rec array_type_of_repr = function
  | LR_java_boolean_array ->
      `Array `Boolean
  | LR_java_byte_array ->
      `Array `Byte
  | LR_java_char_array ->
      `Array `Char
  | LR_java_double_array ->
      `Array `Double
  | LR_java_float_array ->
      `Array `Float
  | LR_java_int_array ->
      `Array `Int
  | LR_java_long_array ->
      `Array `Long
  | LR_java_short_array ->
      `Array `Short
  | LR_java_reference_array (LR_java_instance cn)
  | LR_java_reference_array (LR_java_extends cn) ->
      `Array (`Class cn)
  | LR_java_reference_array LR_value ->
      (* can happen due to 'JavaArray.Reference_array[2]' that accept
         a type parameter 'a that is hence mapped to LR_value *)
      `Array (`Class "java.lang.Object")
  | LR_java_reference_array arr ->
      `Array ((array_type_of_repr arr) :> non_void_java_type)
  | LR_value
  | LR_int
  | LR_char
  | LR_string
  | LR_float
  | LR_bool
  | LR_unit
  | LR_exn
  | LR_array _
  | LR_list _
  | LR_option _
  | LR_nativeint
  | LR_int32
  | LR_int64
  | LR_lazy _
  | LR_none
  | LR_java_instance _
  | LR_java_extends _ ->
      Misc.fatal_error "Macroinstr.array_type_of_repr"

let kind_of_repr = function
  | LR_value | LR_string | LR_exn | LR_array _
  | LR_list _ | LR_option _ | LR_lazy _ ->
      Boxed_value
  | LR_int | LR_char | LR_bool ->
      int_kind
  | LR_float ->
      Unboxed_float
  | LR_nativeint ->
      Unboxed_nativeint
  | LR_int32 ->
      Unboxed_int32
  | LR_int64 ->
      Unboxed_int64
  | LR_java_instance cn | LR_java_extends cn ->
      Unboxed_instance cn
  | (LR_java_boolean_array | LR_java_byte_array
  | LR_java_char_array | LR_java_double_array
  | LR_java_float_array | LR_java_int_array
  | LR_java_long_array | LR_java_reference_array _
  | LR_java_short_array) as arr ->
      Unboxed_java_array (array_type_of_repr arr)
  | LR_unit ->
      Boxed_value
  | LR_none ->
      Misc.fatal_error "Macroinstr.kind_of_repr"

let size_of_value_kind = function
  | Boxed_value          -> 1
  | Tagged_int           -> 2
  | Normalized_int       -> 2
  | Unboxed_int          -> 2
  | Unboxed_int32        -> 1
  | Unboxed_int64        -> 2
  | Unboxed_nativeint    -> 2
  | Unboxed_float        -> 2
  | Unboxed_instance _   -> 1
  | Unboxed_java_array _ -> 1

type parameters =
  | Fixed of value_kind list
  | Unbounded of value_kind

let kind_of_boxed_integer = function
  | Lambda.Pnativeint -> Unboxed_nativeint
  | Lambda.Pint32 -> Unboxed_int32
  | Lambda.Pint64 -> Unboxed_int64

let combine_kinds x y =
  if x = y then
    x
  else
    Boxed_value

let repeat_kind k n =
  let rec build k acc n =
    if n <= 0 then
      acc
    else
      build k (k :: acc) (pred n) in
  build k [] n

type expression =
  | Mconst of Jlambda.const
  | Moffset of expression * int
  | Mdynamicoffset of expression * expression
  | Mcreateglobal of string * string
  | Mclosure of int * mfunction list * expression list
  | Mreadlocal of value_kind * int
  | Mwritelocal of value_kind * int * expression
  | Mpoptolocal of value_kind * int
  | Mcall of Jlambda.function_label * expression list * value_kind list * value_kind option * Debuginfo.t
  | Mprim of Lambda.primitive * expression list * Debuginfo.t
  | Mjavaprim of Jlambda.java_primitive * expression list * Debuginfo.t
  | Mapply of int * expression list * Debuginfo.t
  | Msend of int * int * int * expression list * Debuginfo.t
  | Msequence of expression * expression
  | Mifthenelse of expression * expression * expression
  | Mwhile of expression * expression * Jlambda.jlamdba_loop_inlining_info
  | Mfor of int * Asttypes.direction_flag * int * expression * Jlambda.jlamdba_loop_inlining_info
  | Munit
  | Mboxedunit
  | Mpop of value_kind
  | Mnop
  | Mtrywith of expression * (int option) * expression
  | Mstaticfail of int * expression list
  | Mstaticcatch of int * expression * expression
  | Mswitch of expression * int array * int array * expression array * expression
  | Mconvert of value_kind * value_kind * expression
  | Minit of int option
and mfunction = {
    mfun_label : Jlambda.function_label;
    mfun_ocaml_arity : int;
    mfun_java_arity : int;
  }

type fundecl =
    { fun_name : string;
      fun_args : Ident.t list;
      fun_params : Lambda.repr list;
      fun_return : Lambda.repr;
      fun_body : expression; }
