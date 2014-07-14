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


(* This camlp4 preprocessor is used in order to ensure that referenced
   classes, methods, and fields actually exist.

   The grammar is extended with the following expressions:
   - JAVA_CLASS_NAME "class_name"
     is compiled to a 'Name.for_class' value for the given class;
   - JAVA_GET_STATIC "field_descriptor",
     JAVA_PUT_STATIC "field_descriptor",
     JAVA_GET_FIELD "field_descriptor", and
     JAVA_PUT_FIELD "field_descriptor"
     are compiled to 'Instrtree.t' values containing accesses to the
     given fields;
   - JAVA_INVOKE_STATIC "method_descriptor",
     JAVA_INVOKE_VIRTUAL "method_descriptor", and
     JAVA_INVOKE_SPECIAL "method_descriptor"
     are compiled to 'Instrtree.t' values containing calls to the given
     methods;
   - JAVA_PARENTS "class_name"
     is compiled to a 'Name.for_class list' value containing all the
     parents of the given class.

   Class names should be fully qualified, unless in one of the packages
   listed by the 'open_packages' variable. *)

open Camlp4
open BaristaLibrary

let jar_files = ref []

let loader = ref None

let get_loader () =
  match !loader with
  | Some l -> l
  | None ->
      match !jar_files with
      | [] -> failwith "no jar file has been specified"
      |  files ->
          try
            let cl =
              (ClassPath.get_default ()) @ (List.rev_map Path.make_of_string files)
              |> ClassPath.make_of_path_list
              |> ClassLoader.make_of_class_path in
            loader := Some cl;
            cl
          with e ->
            failwith ("unable to create class loader: " ^ (Printexc.to_string e))

type method_kind =
  | Static
  | Virtual
  | Special

let open_packages =
  [ "java.io" ;
    "java.lang" ;
    "java.lang.invoke" ;
    "java.util" ;
    "org.ocamljava.runtime.annotations.markers" ;
    "org.ocamljava.runtime.annotations.parameters" ;
    "org.ocamljava.runtime.annotations.primitives" ;
    "org.ocamljava.runtime.kernel" ;
    "org.ocamljava.runtime.parameters" ;
    "org.ocamljava.runtime.support.applet" ;
    "org.ocamljava.runtime.support.servlet" ;
    "org.ocamljava.runtime.values" ]
  |> List.map UTF8.of_string

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  (* Returns [Some <variable-for-class-name-as-a-camlp4-expr>] if the
     class name is predefined as a variable.
     Returns [None] otherwise. *)
  let predefined_class str =
    let _loc = Loc.ghost in
    match str with
    | "AbstractNativeRunner"
    | "org.ocamljava.runtime.kernel.AbstractNativeRunner" ->
        Some <:expr< class_AbstractNativeRunner >>
    | "BlockValue"
    | "org.ocamljava.runtime.values.BlockValue" ->
        Some <:expr< class_BlockValue >>
    | "Value"
    | "org.ocamljava.runtime.values.Value" ->
        Some <:expr< class_Value >>
    | _ ->
        None

  (** Returns an expression creating a class name from the passed string.
      The 'opt' parameter indicates whether prebuilt names stored in
      variables should be used. *)
  let make_class_name opt str =
    let _loc = Loc.ghost in
    if opt then
      match predefined_class str with
      | Some expr -> expr
      | None -> <:expr< make_class $str:str$ >>
    else
      <:expr< make_class $str:str$ >>
  let make_class_name' opt cn =
    cn
    |> Name.external_utf8_for_class
    |> UTF8.to_string
    |> make_class_name opt

  (** Returns an expression creating a method name from the passed
      string. *)
  let make_method_name str =
    let _loc = Loc.ghost in
    <:expr< make_method $str:str$ >>
  let make_method_name' mn =
    mn
    |> Name.utf8_for_method
    |> UTF8.to_string
    |> make_method_name

  (** Returns an expression creating a field name from the passed
      string. *)
  let make_field_name str =
    let _loc = Loc.ghost in
    <:expr< make_field $str:str$ >>
  let make_field_name' fn =
    fn
    |> Name.utf8_for_field
    |> UTF8.to_string 
    |> make_field_name

  (** Executes [f l] where [l] is the class loader, handling
      exceptions. *)
  let protect f =
    try
      f (get_loader ())
    with
    | (Failure _) as f -> raise f
    | e -> failwith (Printexc.to_string e)

  (** Constructs a list literal for the values of [l] mapped throuh
      [f]. *)
  let make_list f l =
    let _loc = Loc.ghost in
    List.fold_right
      (fun elem acc ->
        <:expr< $f elem$ :: $acc$ >>)
      l
      <:expr< [] >>

  (** Converts a type descriptor into its camlp4 expression
      equivalent. *)
  let rec make_type (t : Descriptor.java_type) =
    let _loc = Loc.ghost in
    match t with
    | `Boolean  -> <:expr< `Boolean >>
    | `Byte     -> <:expr< `Byte >>
    | `Char     -> <:expr< `Char >>
    | `Double   -> <:expr< `Double >>
    | `Float    -> <:expr< `Float >>
    | `Int      -> <:expr< `Int >>
    | `Long     -> <:expr< `Long >>
    | `Short    -> <:expr< `Short >>
    | `Class cn ->
        let name = UTF8.to_string (Name.external_utf8_for_class cn) in
        begin match predefined_class name with
        | Some cls ->
            <:expr< `Class $cls$ >>
        | None ->
            let cn = make_class_name true name in
            <:expr< `Class $cn$ >>
        end
    | `Array a  -> <:expr< `Array $make_type (a :> Descriptor.java_type)$ >>
    | `Void     -> <:expr< `Void >>

  (** Converts a type descriptor list into its camlp4 expression
      equivalent. *)
  let make_type_list (l : Descriptor.java_type list) =
    make_list make_type l

  (** Converts a method descriptor into its camlp4 expression
      equivalent. *)
  let make_descriptor ((params, ret) : Descriptor.for_method) =
    let _loc = Loc.ghost in
    <:expr< ($make_type_list (params :> Descriptor.java_type list)$,
             $make_type ret$) >>

  (* Implementation for 'JAVA_CLASS_NAME'. *)
  let classn class_name =
    protect
      (fun loader ->
        let found =
          class_name
          |> UTF8.of_string
          |> Lookup.for_class false ~open_packages loader in
        make_class_name' false found.Lookup.value.ClassDefinition.name)

  (* Implementation for 'JAVA_{GET,PUT}_{STATIC,FIELD}'. *)
  let field ~get ~static field_desc =
    protect
      (fun loader ->
        let found =
          (field_desc ^ ":_")
          |> UTF8.of_string
          |> Lookup.for_field false ~open_packages loader in
        let declaring_class, field = found.Lookup.value in
        if (AccessFlag.mem_field `Static field.Field.flags) <> static then
          failwith "invalid field staticness";
        let cn = make_class_name' true declaring_class.ClassDefinition.name in
        let fn = make_field_name' field.Field.name in
        let fd =
          (field.Field.descriptor :> Descriptor.java_type)
          |> make_type in
        let _loc = Loc.ghost in
        let instr =
          match get, static with
          | true, true   -> <:expr< Instruction.GETSTATIC ($cn$, $fn$, $fd$) >>
          | false, true  -> <:expr< Instruction.PUTSTATIC ($cn$, $fn$, $fd$) >>
          | true, false  -> <:expr< Instruction.GETFIELD  ($cn$, $fn$, $fd$) >>
          | false, false -> <:expr< Instruction.PUTFIELD  ($cn$, $fn$, $fd$) >> in
        <:expr< Instrtree.leaf [ $instr$ ] >>)

  (* Implementation for 'JAVA_INVOKE_{STATIC,VIRTUAL,SPECIAL}'. *)
  let methd ~kind method_desc =
    protect
      (fun loader ->
        let _loc = Loc.ghost in
        let method_desc = UTF8.of_string method_desc in
        let instr =
          match kind with
          | Static | Virtual ->
              let static = kind = Static in
              let found = Lookup.for_regular_method false ~open_packages loader method_desc in
              let declaring_class, meth = found.Lookup.value in
              if (AccessFlag.mem_method `Static meth.Method.flags) <> static then
                failwith "invalid method staticness";
              let cn = make_class_name' true declaring_class.ClassDefinition.name in
              let mn = make_method_name' meth.Method.name in
              let md = make_descriptor meth.Method.descriptor in
              if static then
                <:expr< Instruction.INVOKESTATIC ($cn$, $mn$, $md$) >>
              else
                <:expr< Instruction.INVOKEVIRTUAL (`Class_or_interface $cn$, $mn$, $md$) >>
          | Special ->
              let found = Lookup.for_constructor false ~open_packages loader method_desc in
              let declaring_class, meth = found.Lookup.value in
              let cn = make_class_name' true declaring_class.ClassDefinition.name in
              let mn = make_method_name "<init>" in
              let md = make_descriptor (meth.Method.cstr_descriptor, `Void) in
              <:expr< Instruction.INVOKESPECIAL ($cn$, $mn$, $md$) >> in
        <:expr< Instrtree.leaf [ $instr$ ] >>)

  (* Implementation for 'JAVA_PARENTS'. *)
  let parents class_name =
    protect
      (fun loader ->
        let found =
          class_name
          |> UTF8.of_string
          |> Lookup.for_class false ~open_packages loader in
        let parents = Hierarchy.parent_class_names false loader found.Lookup.value in
        make_list
          (fun cn ->
            cn
            |> Name.external_utf8_for_class
            |> UTF8.to_string
            |> make_class_name true)
          parents)

  EXTEND Gram
    GLOBAL: expr;
    expr: LEVEL "simple"
    [[ "JAVA_CLASS_NAME"; class_name = STRING ->
          classn class_name
      | "JAVA_GET_STATIC"; field_desc = STRING ->
          field ~get:true ~static:true field_desc
      | "JAVA_PUT_STATIC"; field_desc = STRING ->
          field ~get:false ~static:true field_desc
      | "JAVA_GET_FIELD"; field_desc = STRING ->
          field ~get:true ~static:false field_desc
      | "JAVA_PUT_FIELD"; field_desc = STRING ->
          field ~get:false ~static:false field_desc
      | "JAVA_INVOKE_STATIC"; method_desc = STRING ->
          methd ~kind:Static method_desc
      | "JAVA_INVOKE_VIRTUAL"; method_desc = STRING ->
          methd ~kind:Virtual method_desc
      | "JAVA_INVOKE_SPECIAL"; method_desc = STRING ->
          methd ~kind:Special method_desc
      | "JAVA_PARENTS"; class_name = STRING ->
          parents class_name ]];
  END
end

let () =
  let module Id = struct
    let name = "Java_pp"
    let version = "internal"
  end in
  let module M = Register.OCamlSyntaxExtension (Id) (Make) in
  Options.add
    "-jar"
    (Arg.String (fun s -> jar_files := s :: !jar_files))
    "<file>  Add the jar file to the classpath"
