(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2014 Xavier Clerc.
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


type conversion_function = JavaAST.expression -> JavaAST.expression

type t = {
    ocaml_type : Path.t option;
    java_type : JavaAST.type_;
    java_of_ocaml : conversion_function;
    ocaml_of_java : conversion_function;
  }

let make_simple ident =
  let path = Path.Pident ident in
  let name = Ident.name ident in
  let info = { ocaml_type = Some path;
               java_type = JavaAST.Reference (name, []);
               java_of_ocaml = (fun e -> JavaAST.Static_call (name, "wrap", [e]));
               ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); } in
  path, info

let type_int =
  { ocaml_type = Some Predef.path_int;
    java_type = JavaAST.Long;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asLong", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createLong", [e])); }

let type_int_boxed =
  { ocaml_type = Some Predef.path_int;
    java_type = JavaAST.Reference ("OCamlInt", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlInt", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_char =
  { ocaml_type = Some Predef.path_char;
    java_type = JavaAST.Int;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asCastedInt", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createLong", [e])); }

let type_char_boxed =
  { ocaml_type = Some Predef.path_char;
    java_type = JavaAST.Reference ("OCamlChar", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlChar", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_string =
  { ocaml_type = Some Predef.path_string;
    java_type = JavaAST.Reference ("String", []);
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asString", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createString", [e])); }

let type_string_boxed =
  { ocaml_type = Some Predef.path_string;
    java_type = JavaAST.Reference ("OCamlString", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlString", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_byte_array =
  { ocaml_type = Some Predef.path_string;
    java_type = JavaAST.Array JavaAST.Byte;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "getBytesForModification", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createString", [e])); }

let type_float =
  { ocaml_type = Some Predef.path_float;
    java_type = JavaAST.Double;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asDouble", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createDouble", [e])); }

let type_float_boxed =
  { ocaml_type = Some Predef.path_float;
    java_type = JavaAST.Reference ("OCamlFloat", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlFloat", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_bool =
  { ocaml_type = Some Predef.path_bool;
    java_type = JavaAST.Boolean;
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlWrappers", "asBool", [e]));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("OCamlWrappers", "createBool", [e])); }

let type_bool_boxed =
  { ocaml_type = Some Predef.path_bool;
    java_type = JavaAST.Reference ("OCamlBool", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlBool", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_unit_boxed =
  { ocaml_type = Some Predef.path_unit;
    java_type = JavaAST.Reference ("OCamlUnit", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlUnit", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_nativeint =
  { ocaml_type = Some Predef.path_nativeint;
    java_type = JavaAST.Long;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asNativeInt", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createNativeInt", [e])); }

let type_nativeint_boxed =
  { ocaml_type = Some Predef.path_nativeint;
    java_type = JavaAST.Reference ("OCamlNativeInt", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlNativeInt", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_int32 =
  { ocaml_type = Some Predef.path_int32;
    java_type = JavaAST.Int;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asInt32", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createInt32", [e])); }

let type_int32_boxed =
  { ocaml_type = Some Predef.path_int32;
    java_type = JavaAST.Reference ("OCamlInt32", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlInt32", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_int64 =
  { ocaml_type = Some Predef.path_int64;
    java_type = JavaAST.Long;
    java_of_ocaml = (fun e -> JavaAST.Call (e, "asInt64", []));
    ocaml_of_java = (fun e -> JavaAST.Static_call ("Value", "createInt64", [e])); }

let type_int64_boxed =
  { ocaml_type = Some Predef.path_int64;
    java_type = JavaAST.Reference ("OCamlInt64", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlInt64", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_array =
  { ocaml_type = Some Predef.path_array;
    java_type = JavaAST.Reference ("OCamlArray", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlArray", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_list =
  { ocaml_type = Some Predef.path_list;
    java_type = JavaAST.Reference ("OCamlList", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlList", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_option =
  { ocaml_type = Some Predef.path_option;
    java_type = JavaAST.Reference ("OCamlOption", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlOption", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_lazy =
  { ocaml_type = Some Predef.path_lazy_t;
    java_type = JavaAST.Reference ("OCamlLazy", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlLazy", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_ref =
  let path_pervasives = Path.Pident (Ident.create_persistent "Pervasives") in
  let path = Path.Pdot (path_pervasives, "ref", 0) in
  { ocaml_type = Some path;
    java_type = JavaAST.Reference ("OCamlRef", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlRef", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_in_channel =
  let path_pervasives = Path.Pident (Ident.create_persistent "Pervasives") in
  let path = Path.Pdot (path_pervasives, "in_channel", 0) in
  { ocaml_type = Some path;
    java_type = JavaAST.Reference ("OCamlInChannel", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlInChannel", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_out_channel =
  let path_pervasives = Path.Pident (Ident.create_persistent "Pervasives") in
  let path = Path.Pdot (path_pervasives, "out_channel", 0) in
  { ocaml_type = Some path;
    java_type = JavaAST.Reference ("OCamlOutChannel", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlOutChannel", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let type_exn =
  { ocaml_type = Some Predef.path_exn;
    java_type = JavaAST.Reference ("OCamlExn", []);
    java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlExn", "wrap", [e]));
    ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); }

let always_predefined_types = [
  type_array;
  type_list;
  type_option;
  type_lazy;
  type_ref;
  type_in_channel;
  type_out_channel;
  type_exn;
]

let boxed_only_predefined_types = [
  type_int_boxed;
  type_char_boxed;
  type_float_boxed;
  type_bool_boxed;
  type_unit_boxed;
  type_nativeint_boxed;
  type_int32_boxed;
  type_int64_boxed;
]

let unboxed_only_predefined_types = [
  type_int;
  type_char;
  type_float;
  type_bool;
  type_nativeint;
  type_int32;
  type_int64;
]

let boxed_predefined_types =
  type_string_boxed
  :: (boxed_only_predefined_types @ always_predefined_types)

let unboxed_predefined_types = ref None

let predefined_types boxed =
  if boxed then
    boxed_predefined_types
  else begin
    match !unboxed_predefined_types with
    | Some cache -> cache
    | None ->
        let res =
          (match !Args.string_mapping with
          | Args.Java_string -> type_string
          | Args.OCamlString -> type_string_boxed
          | Args.Byte_array  -> type_byte_array)
          :: (unboxed_only_predefined_types @ always_predefined_types) in
        unboxed_predefined_types := Some res;
        res
  end
