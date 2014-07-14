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

let java_lang_Object = LR_java_instance "java.lang.Object"

let java_array_primitive_infos = function
  | "java array get boolean"         -> Java_array_get (`Array `Boolean),
                                        LR_bool
  | "java array get byte"            -> Java_array_get (`Array `Byte),
                                        LR_int
  | "java array get char"            -> Java_array_get (`Array `Char),
                                        LR_int
  | "java array get double"          -> Java_array_get (`Array `Double),
                                        LR_float
  | "java array get float"           -> Java_array_get (`Array `Float),
                                        LR_float
  | "java array get int"             -> Java_array_get (`Array `Int),
                                        LR_int32
  | "java array get long"            -> Java_array_get (`Array `Long),
                                        LR_int64
  | "java array get short"           -> Java_array_get (`Array `Short),
                                        LR_int
  | "java array get reference"       -> Java_array_get (`Array (`Class "java.lang.Object")),
                                        java_lang_Object
  | "java array set boolean"         -> Java_array_set (`Array `Boolean),
                                        LR_none
  | "java array set byte"            -> Java_array_set (`Array `Byte),
                                        LR_none
  | "java array set char"            -> Java_array_set (`Array `Char),
                                        LR_none
  | "java array set double"          -> Java_array_set (`Array `Double),
                                        LR_none
  | "java array set float"           -> Java_array_set (`Array `Float),
                                        LR_none
  | "java array set int"             -> Java_array_set (`Array `Int),
                                        LR_none
  | "java array set long"            -> Java_array_set (`Array `Long),
                                        LR_none
  | "java array set short"           -> Java_array_set (`Array `Short),
                                        LR_none
  | "java array set reference"       -> Java_array_set (`Array (`Class "java.lang.Object")),
                                        LR_none
  | "java array length boolean"      -> Java_array_length (`Array `Boolean),
                                        LR_int32
  | "java array length byte"         -> Java_array_length (`Array `Byte),
                                        LR_int32
  | "java array length char"         -> Java_array_length (`Array `Char),
                                        LR_int32
  | "java array length double"       -> Java_array_length (`Array `Double),
                                        LR_int32
  | "java array length float"        -> Java_array_length (`Array `Float),
                                        LR_int32
  | "java array length int"          -> Java_array_length (`Array `Int),
                                        LR_int32
  | "java array length long"         -> Java_array_length (`Array `Long),
                                        LR_int32
  | "java array length short"        -> Java_array_length (`Array `Short),
                                        LR_int32
  | "java array length reference"    -> Java_array_length (`Array (`Class "java.lang.Object")),
                                        LR_int32
  | "java array to_object boolean"   -> Java_array_to_object (`Array `Boolean),
                                        java_lang_Object
  | "java array to_object byte"      -> Java_array_to_object (`Array `Byte),
                                        java_lang_Object
  | "java array to_object char"      -> Java_array_to_object (`Array `Char),
                                        java_lang_Object
  | "java array to_object double"    -> Java_array_to_object (`Array `Double),
                                        java_lang_Object
  | "java array to_object float"     -> Java_array_to_object (`Array `Float),
                                        java_lang_Object
  | "java array to_object int"       -> Java_array_to_object (`Array `Int),
                                        java_lang_Object
  | "java array to_object long"      -> Java_array_to_object (`Array `Long),
                                        java_lang_Object
  | "java array to_object short"     -> Java_array_to_object (`Array `Short),
                                        java_lang_Object
  | "java array to_object reference" -> Java_array_to_object (`Array (`Class "java.lang.Object")),
                                        java_lang_Object
  | "java array of_object boolean"   -> Java_array_of_object (`Array `Boolean),
                                        LR_java_boolean_array
  | "java array of_object byte"      -> Java_array_of_object (`Array `Byte),
                                        LR_java_byte_array
  | "java array of_object char"      -> Java_array_of_object (`Array `Char),
                                        LR_java_char_array
  | "java array of_object double"    -> Java_array_of_object (`Array `Double),
                                        LR_java_double_array
  | "java array of_object float"     -> Java_array_of_object (`Array `Float),
                                        LR_java_float_array
  | "java array of_object int"       -> Java_array_of_object (`Array `Int),
                                        LR_java_int_array
  | "java array of_object long"      -> Java_array_of_object (`Array `Long),
                                        LR_java_long_array
  | "java array of_object short"     -> Java_array_of_object (`Array `Short),
                                        LR_java_short_array
  | "java array of_object reference" -> Java_array_of_object (`Array (`Class "java.lang.Object")),
                                        LR_java_reference_array java_lang_Object
  | _ ->
      raise Not_found

let is_java_array_primitive pname =
  try
    ignore (java_array_primitive_infos pname);
    true
  with Not_found ->
    false

let is_int ty =
  match (Btype.repr ty).Types.desc with
  | Types.Tconstr (path, [], _) ->
      Path.same Predef.path_int path
  | _ -> false

let is_int_list ty =
  match (Btype.repr ty).Types.desc with
  | Types.Tconstr (path, [param], _) ->
      (Path.same Predef.path_list path) && (is_int param)
  | _ -> false

let is_arrow_to_int ty =
  match (Btype.repr ty).Types.desc with
  | Types.Tarrow (_, _, ret, _) ->
      is_int ret
  | _ -> false

let is_arrow_to_arrow_to_int ty =
  match (Btype.repr ty).Types.desc with
  | Types.Tarrow (_, _, ret, _) ->
      is_arrow_to_int ret
  | _ -> false

let optimize_intarray_call fundesc types =
  let class_name = fundesc.fun_label.fl_class in
  let meth_name = fundesc.fun_label.fl_method in
  let meth_name =
    try
      (* remove the trailing "_numberid" part of the name *)
      let idx = String.rindex meth_name '_' in
      String.sub meth_name 0 idx
    with _ ->
      meth_name in
  let return repr_list repr =
    let lbl = { fl_class = "org.ocamljava.runtime.primitives.stdlib.ArrayFunctions";
                fl_method = meth_name ^ "_int" } in
    { fundesc with fun_label = lbl;
      fun_repr_parameters = repr_list;
      fun_repr_return = repr;
      fun_inline = None; } in
  match class_name, meth_name, types with
  | "ocaml.stdlib.Array", "init", [_; Some ty] when is_arrow_to_int ty ->
      return [LR_int; LR_value] LR_value
  | "ocaml.stdlib.Array", ("make_matrix" | "create_matrix"), [_; _; Some ty] when is_int ty ->
      return [LR_int; LR_int; LR_int] LR_value
  | "ocaml.stdlib.Array", "of_list", [Some ty] when is_int_list ty ->
      return [LR_value] LR_value
  | "ocaml.stdlib.Array", "map", [Some ty; _] when is_arrow_to_int ty ->
      return [LR_value; LR_value] LR_value
  | "ocaml.stdlib.Array", "mapi", [Some ty; _] when is_arrow_to_arrow_to_int ty ->
      return [LR_value; LR_value] LR_value
  | _ ->
      fundesc

let optimize_intarray_primitive prim args =
  match prim with
  | Lambda.Pccall prim_desc ->
      let name =
        match prim_desc.Primitive.prim_name, args with
        | "caml_make_vect", [_; (_, Some ty)] when is_int ty ->
            "caml_make_vect_int"
        | x, _ ->
            (* "caml_array_sub", "caml_array_append", and "caml_array_concat"
               are specialized in the runtime *)
            x in
      Lambda.Pccall { prim_desc with Primitive.prim_name = name }
  | _ ->
      prim
