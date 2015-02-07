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

open BaristaLibrary
open Bytecodeutils


type error =
  | Unavailable_primitive_definitions
  | Invalid_primitive_definitions
  | Undefined_class of string * (string option)
  | Undefined_primitive of string

exception Error of error

type description = {
    primdesc_class : string;
    primdesc_method : string;
    primdesc_parameters : Lambda.repr list;
    primdesc_return : Lambda.repr;
    primdesc_javadesc : Descriptor.for_method;
  }

(* tests whether a class definition is a primitive provider *)
let is_primitive_provider cdef =
  try
    (cdef.ClassDefinition.attributes :> Attribute.t list)
    |> Attribute.extract_annotations
    |> List.exists
        (fun (annot_class_name, _) ->
          Name.equal_for_class class_PrimitiveProvider annot_class_name)
  with _ ->
    false

(* returns an option containing the primitive annotation, if any *)
let extract_primitive_annotation meth =
  try
    Some ((meth.Method.attributes :> Attribute.t list)
          |> Attribute.extract_annotations
          |> List.find
              (fun (annot_class_name, _) ->
                Name.equal_for_class class_Primitive annot_class_name)
          |> snd)
  with _ ->
    None

(* map from primitive name to primitive description *)
let table : (string, description) Hashtbl.t = Hashtbl.create 1307

(* converts Java type to repr as would be stored in 'ocamlrun.primitives',
   keep synchronized with Primitives2Marshalled. *)
let rec repr_of_type_and_annot : Descriptor.java_type -> string -> Lambda.repr =
  fun java_type annot ->
    let open Lambda in
    match java_type, annot with
    | `Boolean, _        -> assert false
    | `Byte, _           -> assert false
    | `Char, _           -> assert false
    | `Double, _         -> LR_float
    | `Float, _          -> assert false
    | `Int, _            -> LR_int32
    | `Long, "int"       -> LR_int
    | `Long, "int64"     -> LR_int64
    | `Long, "nativeint" -> LR_nativeint
    | `Long, "char"      -> LR_char
    | `Long, "bool"      -> LR_bool
    | `Long, "unit"      -> LR_unit
    | `Long, _           -> assert false
    | `Short, _          -> assert false
    | `Void, _           -> LR_none
    | `Array `Boolean, _ -> LR_java_boolean_array
    | `Array `Byte, _    -> LR_java_byte_array
    | `Array `Char, _    -> LR_java_char_array
    | `Array `Double, _  -> LR_java_double_array
    | `Array `Float, _   -> LR_java_float_array
    | `Array `Int, _     -> LR_java_int_array
    | `Array `Long, _    -> LR_java_long_array
    | `Array `Short, _   -> LR_java_short_array
    | `Array arr, _      ->
        let arr = (arr :> Descriptor.java_type) in
        LR_java_reference_array (repr_of_type_and_annot arr annot)
    | `Class cn, _       ->
        if Name.equal_for_class class_Value cn then
          LR_value
        else
          LR_java_instance (UTF8.to_string (Name.external_utf8_for_class cn))

(* adds to the table the primitives from the class whose name is passed *)
let visit loader class_name =
  let cdef =
    try
      class_name
      |> UTF8.of_string
      |> ClassLoader.find_class loader
    with
    | ClassLoader.Exception (ClassLoader.Unable_to_load (_, err)) ->
        raise (Error (Undefined_class (class_name, Some err)))
    | ClassLoader.Exception err ->
        let msg = ClassLoader.string_of_error err in
        raise (Error (Undefined_class (class_name, Some msg)))
    | _ ->
        raise (Error (Undefined_class (class_name, None))) in
  let class_name =
    cdef.ClassDefinition.name
    |> Name.external_utf8_for_class
    |> UTF8.to_string in
  if is_primitive_provider cdef then begin
    if !Jclflags.dump_primitives then
      Printf.printf "primitives from %s:\n" class_name;
    List.iter
      (function
        | Method.Regular meth ->
            (match extract_primitive_annotation meth with
            | Some annot_bindings ->
                let get_binding bindings name =
                  List.find
                    (fun (n, _) ->
                      (UTF8.to_string n) = name)
                    bindings
                  |> snd in
                let string_of_annotation_value = function
                  | Annotation.String_value str -> UTF8.to_string str
                  | _ -> assert false in
                let method_name =
                  meth.Method.name
                  |> Name.utf8_for_method
                  |> UTF8.to_string in
                let parameters_annot =
                  match get_binding annot_bindings "parameterTypes" with
                  | Annotation.Array_value l -> List.map string_of_annotation_value l
                  | _ -> assert false in
                let return_annot =
                  get_binding annot_bindings "returnType"
                  |> string_of_annotation_value in
                if !Jclflags.dump_primitives then
                  Printf.printf "  - %s\n" method_name;
                let parameters, return = meth.Method.descriptor in
                assert (List.length parameters = List.length parameters_annot);
                let params =
                  List.map2
                    repr_of_type_and_annot
                    (parameters :> Descriptor.java_type list)
                    parameters_annot in
                let ret = repr_of_type_and_annot return return_annot in
                let desc =
                  { primdesc_class = class_name;
                    primdesc_method = method_name;
                    primdesc_parameters = params;
                    primdesc_return = ret;
                    primdesc_javadesc = meth.Method.descriptor; } in
                Hashtbl.add table method_name desc
            | None -> ())
        | Method.Constructor _
        | Method.Initializer _ -> ())
      cdef.ClassDefinition.methods
  end;
  flush stdout

(* converts repr stored in 'ocamlrun.primitives' to actual Java type,
   keep synchronized with Primitives2Marshalled. *)
let rec java_type_of_repr : Lambda.repr -> Descriptor.java_type =
  fun repr ->
    let mk_class cn =
      `Class (Name.make_for_class_from_external (UTF8.of_string cn)) in
    let open Lambda in
    match repr with
    | LR_value                    -> `Class class_Value
    | LR_int                      -> `Long
    | LR_char                     -> `Long
    | LR_string                   -> `Class class_Value
    | LR_float                    -> `Double
    | LR_bool                     -> `Long
    | LR_unit                     -> `Long
    | LR_exn                      -> `Class class_Value
    | LR_array _                  -> `Class class_Value
    | LR_list _                   -> `Class class_Value
    | LR_option _                 -> `Class class_Value
    | LR_nativeint                -> `Long
    | LR_int32                    -> `Int
    | LR_int64                    -> `Long
    | LR_lazy _                   -> `Class class_Value
    | LR_java_instance cn         -> mk_class cn
    | LR_java_extends cn          -> mk_class cn
    | LR_java_boolean_array       -> `Array `Boolean
    | LR_java_byte_array          -> `Array `Byte
    | LR_java_char_array          -> `Array `Char
    | LR_java_double_array        -> `Array `Double
    | LR_java_float_array         -> `Array `Float
    | LR_java_int_array           -> `Array `Int
    | LR_java_long_array          -> `Array `Long
    | LR_java_reference_array arr -> `Array (non_void_java_type_of_repr arr)
    | LR_java_short_array         -> `Array `Short
    | LR_none                     -> `Void
and non_void_java_type_of_repr : Lambda.repr -> Descriptor.non_void_java_type =
  fun repr ->
    repr
    |> java_type_of_repr
    |> Descriptor.filter_void Descriptor.Void_not_allowed
and java_type_of_repr_list l =
  List.fold_right
    (fun repr acc ->
      try
        (non_void_java_type_of_repr repr) :: acc
      with _ ->
        acc)
    l
    []

(* the type for the values in 'ocamlrun.primitives' *)
type marshalled_desc = marshalled_class_desc array
and marshalled_class_desc = {
    mcd_class_name : string;
    mcd_methods : marshalled_method_desc array;
  }
and marshalled_method_desc = {
    mmd_method_name : string;
    mmd_parameters : Lambda.repr array;
    mmd_return : Lambda.repr;
  }

let initialized = ref false

(* get primitives from 'ocamlrun.primitives' and additional providers,
   according to command-line flags *)
let init () =
  if not !initialized then begin
    initialized := true; (* if an exception is raised, do not re-run *)
    if not (!Jclflags.nobuiltin) then begin
      let file =
        try
          Misc.find_in_path
            (Config.standard_library :: !Config.load_path)
            Jconfig.primitive_definitions
        with Not_found ->
          raise (Error Unavailable_primitive_definitions) in
      let chan = open_in file in
      let defs : marshalled_desc =
        try
          Marshal.from_channel chan
        with _ ->
          raise (Error Invalid_primitive_definitions) in
      Array.iter
        (fun { mcd_class_name; mcd_methods } ->
          if !Jclflags.dump_primitives then
            Printf.printf "builtin primitives from %s:\n" mcd_class_name;
          Array.iter
            (fun { mmd_method_name; mmd_parameters; mmd_return } ->
              let mmd_parameters = Array.to_list mmd_parameters in
              if !Jclflags.dump_primitives then
                Printf.printf "  - %s\n" mmd_method_name;
              let meth_java_desc =
                java_type_of_repr_list mmd_parameters,
                java_type_of_repr mmd_return in
              let desc =
                { primdesc_class = mcd_class_name;
                  primdesc_method = mmd_method_name;
                  primdesc_parameters = mmd_parameters;
                  primdesc_return = mmd_return;
                  primdesc_javadesc = meth_java_desc } in
              Hashtbl.add table mmd_method_name desc)
            mcd_methods)
        defs;
      close_in chan
    end;
    let loader = Jutils.get_class_loader () in
    !Jclflags.providers
    |> List.rev
    |> List.iter (fun cn -> visit loader cn);
  end

(* first look for unboxed primitives, then for boxed one *)
let get_description name =
  init ();
  try
    try
      Hashtbl.find table (name ^ "$")
    with Not_found ->
      Hashtbl.find table name
  with Not_found ->
    raise (Error (Undefined_primitive name))

let report_error ppf = function
  | Unavailable_primitive_definitions ->
      Format.fprintf ppf "File %a cannot be found in load path"
        Location.print_filename Jconfig.primitive_definitions
  | Invalid_primitive_definitions ->
      Format.fprintf ppf "File %a cannot be loaded"
        Location.print_filename Jconfig.primitive_definitions
  | Undefined_class (cn, None) ->
      Format.fprintf ppf "Class '%s' cannot be found in classpath" cn
  | Undefined_class (cn, (Some msg)) ->
      Format.fprintf ppf "Class '%s' cannot be loaded:@ %s" cn msg
  | Undefined_primitive prim ->
      Format.fprintf ppf "Primitive '%s' cannot be found" prim
