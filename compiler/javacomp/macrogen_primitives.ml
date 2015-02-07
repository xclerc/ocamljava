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

open Lambda
open Jlambda
open Macroinstr


let unary_math_primitives = [
  "caml_exp_float",      "exp" ;
  "caml_expm1_float",    "expm1" ;
  "caml_acos_float",     "acos" ;
  "caml_asin_float",     "asin" ;
  "caml_atan_float",     "atan" ;
  "caml_cos_float",      "cos" ;
  "caml_cosh_float",     "cosh" ;
  "caml_log_float",      "log" ;
  "caml_log10_float",    "log10" ;
  "caml_log1p_float",    "log1p" ;
  "caml_sin_float",      "sin" ;
  "caml_sinh_float",     "sinh" ;
  "caml_sqrt_float",     "sqrt" ;
  "caml_tan_float",      "tan" ;
  "caml_tanh_float",     "tanh" ;
  "caml_ceil_float",     "ceil" ;
  "caml_floor_float",    "floor" ;
  "caml_abs_float",      "abs"
]

let is_unary_math_primitive x =
  List.mem_assoc x unary_math_primitives

let unary_method x =
  List.assoc x unary_math_primitives

let binary_math_primitives = [
  "caml_power_float",    "pow" ;
  "caml_atan2_float",    "atan2" ;
  "caml_hypot_float",    "hypot" ;
  "caml_copysign_float", "copysign"
]

let is_binary_math_primitive x =
  List.mem_assoc x binary_math_primitives

let binary_method x =
  List.assoc x binary_math_primitives

let signature_of_primdesc desc =
  begin match desc.Runtimeprimitives.primdesc_parameters with
  | (_ :: _) as l -> Fixed (List.map kind_of_repr l)
  | []            -> Fixed [Boxed_value]
  end,
  begin match desc.Runtimeprimitives.primdesc_return with
  | LR_none -> Boxed_value
  | r       -> kind_of_repr r
  end

let kind_of_approx = function
  | Value_closure _           -> Boxed_value
  | Value_tuple _             -> Boxed_value
  | Value_unknown (Some repr) -> kind_of_repr repr
  | Value_unknown None        -> Boxed_value
  | Value_integer _           -> int_kind
  | Value_integer32 _         -> Unboxed_int32
  | Value_integer64 _         -> Unboxed_int64
  | Value_integernat _        -> Unboxed_nativeint
  | Value_constptr _          -> int_kind
  | Value_float _             -> Unboxed_float
  | Value_java_null (Some t)  ->
      begin match t with
      | `Class cn  -> Unboxed_instance cn
      | `Array arr -> Unboxed_java_array (`Array arr)
      | `Boolean | `Byte | `Char | `Double | `Float | `Int | `Long | `Short ->
          assert false
      end
  | Value_java_null None      -> Unboxed_instance "java.lang.Object"
  | Value_java_string _       -> Unboxed_instance "java.lang.String"

let int_op_kind =
  if Jconfig.ints_are_63_bit_long then
    Tagged_int
  else
    Unboxed_int

let bool_kind = Normalized_int

let string_kind = Boxed_value

let array_kind = Boxed_value

let big_array_kind = Boxed_value

let signature_of_primitive prim args =
  let values n = Fixed (repeat_kind Boxed_value n), Boxed_value in
  let unary k = Fixed [k], k in
  let binary k = Fixed [k; k], k in
  let shift k = Fixed [k; Normalized_int], k in
  let comparison k = Fixed [k; k], bool_kind in
  let unary_bi bi = unary (kind_of_boxed_integer bi) in
  let binary_bi bi = binary (kind_of_boxed_integer bi) in
  let shift_bi bi = shift (kind_of_boxed_integer bi) in
  let comparison_bi bi = comparison (kind_of_boxed_integer bi) in
  let fatal_error s =
    Misc.fatal_error ("Macrogen_primitives.signature_of_primitive: " ^ s) in
  match prim with
  | Pidentity -> fatal_error "Pidentity"
  | Pignore -> fatal_error "Pignore"
  | Prevapply _ -> fatal_error "Prevapply"
  | Pdirapply _ -> fatal_error "Pdirapply"
  | Pgetglobal _ -> Fixed [], Boxed_value
  | Psetglobal _ -> Fixed [Boxed_value], Boxed_value
  | Pmakeblock _ -> Unbounded Boxed_value, Boxed_value
  | Pfield idx ->
      begin match args with
      | [Jprim (Pgetglobal id, _, _)] ->
          begin try
            match Jcompilenv.global_approx_no_dep id with
            | Value_tuple approx when idx < (Array.length approx) ->
                Fixed [Boxed_value], kind_of_approx approx.(idx)
            | _ ->
                Fixed [Boxed_value], Boxed_value
          with _ ->
            Fixed [Boxed_value], Boxed_value
          end
      | _ -> Fixed [Boxed_value], Boxed_value
      end
  | Psetfield (idx, _) ->
      begin match args with
      | [Jprim (Pgetglobal id, _, _); _] ->
          begin try
            match Jcompilenv.global_approx_no_dep id with
            | Value_tuple approx when idx < (Array.length approx) ->
                Fixed [Boxed_value; kind_of_approx approx.(idx)], Boxed_value
            | _ -> Fixed [Boxed_value; Boxed_value], Boxed_value
          with _ ->
            Fixed [Boxed_value; Boxed_value], Boxed_value
          end
      | _ -> Fixed [Boxed_value; Boxed_value], Boxed_value
      end
  | Pfloatfield _ -> Fixed [Boxed_value], Unboxed_float
  | Psetfloatfield _ -> Fixed [Boxed_value; Unboxed_float], Boxed_value
  | Pduprecord _ -> Fixed [Boxed_value], Boxed_value
  | Plazyforce -> fatal_error "Plazyforce"
  | Pccall prim when is_unary_math_primitive prim.Primitive.prim_name ->
      unary Unboxed_float
  | Pccall prim when is_binary_math_primitive prim.Primitive.prim_name ->
      binary Unboxed_float
  | Pccall prim ->
      prim.Primitive.prim_name
      |> Runtimeprimitives.get_description
      |> signature_of_primdesc
  | Praise -> Fixed [Boxed_value], Boxed_value
  | Psequand -> binary bool_kind
  | Psequor -> binary bool_kind
  | Pnot -> unary bool_kind
  | Pnegint -> unary int_op_kind
  | Paddint -> binary int_op_kind
  | Psubint -> binary int_op_kind
  | Pmulint -> binary int_op_kind
  | Pdivint -> binary int_op_kind
  | Pmodint -> binary int_op_kind
  | Pandint -> binary int_op_kind
  | Porint -> binary int_op_kind
  | Pxorint -> binary int_op_kind
  | Plslint -> shift int_op_kind
  | Plsrint -> shift int_op_kind
  | Pasrint -> shift int_op_kind
  | Pintcomp _ -> comparison int_op_kind
  | Poffsetint _ -> Fixed [int_op_kind], int_op_kind
  | Poffsetref _ -> Fixed [Boxed_value], Boxed_value
  | Pintoffloat -> Fixed [Unboxed_float], Normalized_int
  | Pfloatofint -> Fixed [Normalized_int], Unboxed_float
  | Pnegfloat -> unary Unboxed_float
  | Pabsfloat -> unary Unboxed_float
  | Paddfloat -> binary Unboxed_float
  | Psubfloat -> binary Unboxed_float
  | Pmulfloat -> binary Unboxed_float
  | Pdivfloat -> binary Unboxed_float
  | Pfloatcomp _ -> comparison Unboxed_float
  | Pstringlength -> Fixed [string_kind], Normalized_int
  | Pstringrefu -> Fixed [string_kind; Normalized_int], Normalized_int
  | Pstringsetu -> Fixed [string_kind; Normalized_int; Normalized_int], Boxed_value
  | Pstringrefs -> Fixed [string_kind; Normalized_int], Normalized_int
  | Pstringsets -> Fixed [string_kind; Normalized_int; Normalized_int], Boxed_value
  | Pmakearray Pintarray -> Unbounded int_op_kind, array_kind
  | Pmakearray Pfloatarray -> Unbounded Unboxed_float, array_kind
  | Pmakearray _ -> Unbounded Boxed_value, array_kind
  | Parraylength _ -> Fixed [array_kind], Normalized_int
  | Parrayrefu Pintarray -> Fixed [array_kind; Normalized_int], int_op_kind
  | Parrayrefu Pfloatarray -> Fixed [array_kind; Normalized_int], Unboxed_float
  | Parrayrefu Pgenarray -> Fixed [array_kind; Normalized_int], Boxed_value
  | Parrayrefu Paddrarray -> Fixed [array_kind; Normalized_int], Boxed_value
  | Parraysetu Pintarray -> Fixed [array_kind; Normalized_int; int_op_kind], Boxed_value
  | Parraysetu Pfloatarray -> Fixed [array_kind; Normalized_int; Unboxed_float], Boxed_value
  | Parraysetu Pgenarray -> Fixed [array_kind; Normalized_int; Boxed_value], Boxed_value
  | Parraysetu Paddrarray -> Fixed [array_kind; Normalized_int; Boxed_value], Boxed_value
  | Parrayrefs Pintarray -> Fixed [array_kind; Normalized_int], int_op_kind
  | Parrayrefs Pfloatarray  -> Fixed [array_kind; Normalized_int], Unboxed_float
  | Parrayrefs Pgenarray -> Fixed [array_kind; Normalized_int], Boxed_value
  | Parrayrefs Paddrarray -> Fixed [array_kind; Normalized_int], Boxed_value
  | Parraysets Pintarray -> Fixed [array_kind; Normalized_int; int_op_kind], Boxed_value
  | Parraysets Pfloatarray -> Fixed [array_kind; Normalized_int; Unboxed_float], Boxed_value
  | Parraysets Pgenarray -> Fixed [array_kind; Normalized_int; Boxed_value], Boxed_value
  | Parraysets Paddrarray -> Fixed [array_kind; Normalized_int; Boxed_value], Boxed_value
  | Pisint -> Fixed [Boxed_value], bool_kind
  | Pisout -> Fixed [int_op_kind; int_op_kind], bool_kind
  | Pbittest -> Fixed [Boxed_value; Normalized_int], bool_kind
  | Pbintofint bi -> Fixed [int_op_kind], kind_of_boxed_integer bi
  | Pintofbint bi -> Fixed [kind_of_boxed_integer bi], int_op_kind
  | Pcvtbint (src, dst) -> Fixed [kind_of_boxed_integer src], kind_of_boxed_integer dst
  | Pnegbint bi -> unary_bi bi
  | Paddbint bi -> binary_bi bi
  | Psubbint bi -> binary_bi bi
  | Pmulbint bi -> binary_bi bi
  | Pdivbint bi -> binary_bi bi
  | Pmodbint bi -> binary_bi bi
  | Pandbint bi -> binary_bi bi
  | Porbint bi -> binary_bi bi
  | Pxorbint bi -> binary_bi bi
  | Plslbint bi -> shift_bi bi
  | Plsrbint bi -> shift_bi bi
  | Pasrbint bi -> shift_bi bi
  | Pbintcomp (bi, _) -> comparison_bi bi
  | Pbigarrayref (_, n, _, _) -> values (succ n)
  | Pbigarrayset (_, n, _, _) -> values (succ (succ n))
  | Pbigarraydim _ -> Fixed [Boxed_value], Boxed_value
  | Pstring_load_16 _ -> Fixed [string_kind; Boxed_value], Boxed_value
  | Pstring_load_32 _ -> Fixed [string_kind; Boxed_value], Boxed_value
  | Pstring_load_64 _ -> Fixed [string_kind; Boxed_value], Boxed_value
  | Pstring_set_16 _ -> Fixed [string_kind; Boxed_value; Boxed_value], Boxed_value
  | Pstring_set_32 _ -> Fixed [string_kind; Boxed_value; Boxed_value], Boxed_value
  | Pstring_set_64 _ -> Fixed [string_kind; Boxed_value; Boxed_value], Boxed_value
  | Pbigstring_load_16 _ -> Fixed [big_array_kind; Boxed_value], Boxed_value
  | Pbigstring_load_32 _ -> Fixed [big_array_kind; Boxed_value], Boxed_value
  | Pbigstring_load_64 _ -> Fixed [big_array_kind; Boxed_value], Boxed_value
  | Pbigstring_set_16 _ -> Fixed [big_array_kind; Boxed_value; Boxed_value], Boxed_value
  | Pbigstring_set_32 _ -> Fixed [big_array_kind; Boxed_value; Boxed_value], Boxed_value
  | Pbigstring_set_64 _ -> Fixed [big_array_kind; Boxed_value; Boxed_value], Boxed_value
  | Pctconst Big_endian -> Fixed [Boxed_value], int_kind
  | Pctconst Word_size -> Fixed [Boxed_value], int_kind
  | Pctconst Ostype_unix -> Fixed [Boxed_value], int_kind
  | Pctconst Ostype_win32 -> Fixed [Boxed_value], int_kind
  | Pctconst Ostype_cygwin -> Fixed [Boxed_value], int_kind
  | Pbswap16 -> Fixed [int_op_kind], int_op_kind
  | Pbbswap bi -> unary_bi bi

let simplif_primitive = function
  | Pduprecord _ -> Some "caml_obj_dup"
  | Pbigarrayref (_, n, _, _) -> Some ("caml_ba_get_" ^ string_of_int n)
  | Pbigarrayset (_, n, _, _) -> Some ("caml_ba_set_" ^ string_of_int n)
  | _ -> None

let kind_of_java_type : Jlambda.java_type -> value_kind = function
  | `Boolean   -> int_kind
  | `Byte      -> int_kind
  | `Char      -> int_kind
  | `Double    -> Unboxed_float
  | `Float     -> Unboxed_float
  | `Int       -> Unboxed_int32
  | `Long      -> Unboxed_int64
  | `Short     -> int_kind
  | `Class cn  -> Unboxed_instance cn
  | `Array arr -> Unboxed_java_array (`Array arr)
  | `Void      -> Boxed_value
let kinds_of_java_types : Jlambda.non_void_java_type list -> value_kind list =
  fun l -> List.map (fun x -> kind_of_java_type (x :> Jlambda.java_type)) l

let signature_of_java_primitive = function
  | Java_constructor (class_name, types, _) ->
      let kinds =
        if types = [] then
          [Boxed_value]
        else
          kinds_of_java_types types in
      kinds, Unboxed_instance class_name
  | Java_array (typ, { jpad_total = _; jpad_init }) ->
      let rec int32s n =
        if n <= 0 then
          []
        else
          Unboxed_int32 :: (int32s (pred n)) in
      int32s jpad_init, kind_of_java_type (typ :> Jlambda.java_type)
  | Java_array_get ((`Array elem) as typ) ->
      [kind_of_java_type (typ :> Jlambda.java_type); Unboxed_int32],
      kind_of_java_type (elem :> Jlambda.java_type)
  | Java_array_set ((`Array elem) as typ) ->
      [kind_of_java_type (typ :> Jlambda.java_type); Unboxed_int32;  kind_of_java_type (elem :> Jlambda.java_type)],
      Boxed_value
  | Java_array_length typ ->
      [kind_of_java_type (typ :> Jlambda.java_type)], Unboxed_int32
  | Java_array_to_object typ ->
      [kind_of_java_type (typ :> Jlambda.java_type)], Unboxed_instance "java.lang.Object"
  | Java_array_of_object typ ->
      [Unboxed_instance "java.lang.Object"], kind_of_java_type (typ :> Jlambda.java_type)
  | Java_method (class_name, _, post_call, call_kind, params, _, return) ->
      let kinds =
        match call_kind, params with
        | Static_call, [] -> [Boxed_value]
        | Static_call, _  -> kinds_of_java_types params
        | _               -> kinds_of_java_types ((`Class class_name) :: params) in
      let ret_kind =
        match post_call with
        | Jtypes.Bare_call     -> kind_of_java_type return
        | Jtypes.Pop_result    -> kind_of_java_type `Void
        | Jtypes.Push_instance -> kind_of_java_type (`Class class_name) in
      kinds, ret_kind
  | Java_get (_, _, Static_field, typ) ->
      [Boxed_value], kind_of_java_type (typ :> java_type)
  | Java_get (class_name, _, Instance_field, typ) ->
      [Unboxed_instance class_name], kind_of_java_type (typ :> java_type)
  | Java_set (_, _, Static_field, typ) ->
      [kind_of_java_type (typ :> java_type)], Boxed_value
  | Java_set (class_name, _, Instance_field, typ) ->
      [Unboxed_instance class_name; kind_of_java_type (typ :> java_type)], Boxed_value
  | Java_get_null ->
      [Boxed_value], Unboxed_instance "java.lang.Object"
  | Java_is_null ->
      [Unboxed_instance "java.lang.Object"], bool_kind
  | Java_is_not_null ->
      [Unboxed_instance "java.lang.Object"], bool_kind
  | Java_equal ->
      [Unboxed_instance "java.lang.Object"; Unboxed_instance "java.lang.Object"], bool_kind
  | Java_not_equal ->
      [Unboxed_instance "java.lang.Object"; Unboxed_instance "java.lang.Object"], bool_kind
  | Java_instanceof _ ->
      [Unboxed_instance "java.lang.Object"], bool_kind
  | Java_cast typ ->
      [Unboxed_instance "java.lang.Object"], kind_of_java_type (typ :> java_type)
  | Java_class _ ->
      [Boxed_value], Unboxed_instance "java.lang.Class"
  | Java_throw ->
      [Unboxed_instance "java.lang.Throwable"], Boxed_value
  | Java_synchronized _ ->
      [Unboxed_instance "java.lang.Object"; Boxed_value], Boxed_value
  | Java_proxy { jpp_kind; jpp_interface; jpp_interfaces = _; jpp_mapping = _ } ->
      begin match jpp_kind with
      | Custom_class_loader ->
          [Unboxed_instance "java.lang.ClassLoader"; Boxed_value], Unboxed_instance jpp_interface
      | System_class_loader ->
          [Boxed_value], Unboxed_instance jpp_interface
      | Runtime_class_loader ->
          [Boxed_value], Unboxed_instance jpp_interface
      end
