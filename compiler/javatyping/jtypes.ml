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

(* Utilities *)

let generics_not_available () =
  Misc.fatal_error "generics are not available in this version"

let rec is_instance_or_extends ty =
  let ty = Btype.repr ty in
  let open Types in
  match ty.desc with
  | Tconstr (p, _, _) ->
      (Path.same Predef.path_java_instance p)
    || (Path.same Predef.path_java_extends p)
  | Tlink ty -> is_instance_or_extends ty
  | _ -> false

let is_special_primitive = function
  | "java constructor"
  | "java make array"
  | "java make array dims"
  | "java method call"
  | "java field get"
  | "java field set"
  | "java instanceof"
  | "java cast"
(*| "java class" is a special primitive, but its parameter is not erased *)
  | "java proxy loader" -> true
  | "java proxy system" -> true
  | "java proxy runtime" -> true
  | _ -> false

let use_dots s =
  let len = String.length s in
  let res = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    match s.[!i] with
    | '\'' when (!i + 1 < len) && (s.[!i + 1] = '\'') ->
        Buffer.add_char res '$';
        i := !i + 2
    | '\'' ->
        Buffer.add_char res '.';
        incr i
    | ch ->
        Buffer.add_char res ch;
        incr i
  done;
  Buffer.contents res

let use_single_quotes s =
  let len = String.length s in
  let res = Buffer.create len in
  for i = 0 to pred len do
    match s.[i] with
    | '.' -> Buffer.add_char res '\''
    | '$' -> Buffer.add_char res '\''; Buffer.add_char res '\''
    | ch  -> Buffer.add_char res ch
  done;
  Buffer.contents res

let contains_dots s =
  let i = ref (String.length s - 3) in
  while (!i >= 0)
      && ((s.[!i] <> '.') || (s.[!i + 1] <> '.') || (s.[!i + 2] <> '.')) do
    decr i
  done;
  !i >= 0


(* Packages *)

let before_any =
  let pos = { Lexing.pos_fname = "";
  	      Lexing.pos_lnum = 0;
  	      Lexing.pos_bol = 0;
  	      Lexing.pos_cnum = 0; } in  
  { Location.loc_start = pos;
    Location.loc_end = pos;
    Location.loc_ghost = false; }

open BaristaLibrary

let java_lang = UTF8.of_string "java.lang"

let opened_packages : (UTF8.t * Location.t) list ref =
  ref [ java_lang, before_any ]

let reset_opened_packages () =
  opened_packages := [ java_lang, before_any ]

let open_package package location =
  opened_packages := (UTF8.of_string (use_dots package), location) :: !opened_packages

let get_opened_packages location =
  (* due to (weird) line directives, packages may not be ordered *)
  let (>=) loc1 loc2 =
    let open Location in
    let open Lexing in
    (loc1.loc_start.pos_lnum > loc2.loc_start.pos_lnum)
    || ((loc1.loc_start.pos_lnum = loc2.loc_start.pos_lnum)
          && (loc1.loc_start.pos_cnum > loc2.loc_start.pos_cnum)) in
  !opened_packages
  |> List.filter (fun (_pack, loc) -> location >= loc)
  |> List.map fst


(* Conversions between classes and tags *)

let deprecated =
  "java.lang.Deprecated"
  |> UTF8.of_string
  |> Name.make_for_class_from_external

let check_deprecated loc (attrs : Attribute.t list) =
  let deprecated =
    (List.exists (fun a -> Attribute.equal `Deprecated a) attrs)
  || (List.exists
        (fun (name, elems) ->
          (Name.equal_for_class name deprecated) && (elems = []))
    (Attribute.extract_annotations attrs)) in
  if deprecated then
    Location.prerr_warning loc Warnings.Java_deprecated

let check_public_class loc cd =
  if not (AccessFlag.mem_class `Public cd.ClassDefinition.access_flags) then
    failwith "class is not public";
  check_deprecated loc (cd.ClassDefinition.attributes :> Attribute.t list)

let rec interface_parents cl cd =
  let parent_definitions =
    List.map
      (fun cn ->
        let cn = Name.external_utf8_for_class cn in
        try
          ClassLoader.find_class cl cn
        with _ ->
          let cn = UTF8.to_string cn in
          let msg = Printf.sprintf "unable to load parent interface %S" cn in
          failwith msg)
      cd.ClassDefinition.implements in
  let tail =
    parent_definitions
    |> List.map (fun p -> interface_parents cl p)
    |> List.flatten in
  cd :: tail
  
module StringSet = Set.Make (String)

let rec add_all_parents cl cd acc =
  let name =
    cd.ClassDefinition.name
    |> Name.external_utf8_for_class
    |>  UTF8.to_string
    |> use_single_quotes in
  let acc =
    acc
    |> StringSet.add name
    |> add_all_parents_list cl cd.ClassDefinition.implements in
  match cd.ClassDefinition.extends with
  | Some parent_name ->
      let parent =
        parent_name
        |> Name.external_utf8_for_class
        |> ClassLoader.find_class cl in
      add_all_parents cl parent acc
  | None ->
      acc
and add_all_parents_list cl l acc =
  List.fold_left
    (fun acc parent_name ->
      let parent =
        parent_name
        |> Name.external_utf8_for_class
        |> ClassLoader.find_class cl in
      add_all_parents cl parent acc)
    acc
    l

let tags_of_class class_name location =
  let len = String.length class_name in
  let implicit_package_prefix =
    (len > 2)
      && (class_name.[0] = '_')
      && (class_name.[1] = '\'') in
  let open_packages =
    if implicit_package_prefix then
      get_opened_packages location
    else
      [] in
  let class_name =
    if implicit_package_prefix then
      String.sub class_name 2 (len - 2)
    else
      class_name in
  let class_name = use_dots class_name in
  let class_loader = Jutils.get_class_loader () in
  try
    let results =
      Lookup.for_classes
        false
        ~open_packages
        class_loader
        (UTF8.of_string class_name) in
    match results with
    | [] ->
        raise Not_found
    | [ result ] ->
        let def = result.Lookup.value in
        if not (AccessFlag.mem_class `Public def.ClassDefinition.access_flags) then
          failwith "visibility";
        check_deprecated location (def.ClassDefinition.attributes :> Attribute.t list);
        add_all_parents class_loader def StringSet.empty
        |> StringSet.elements
    | _ ->
        raise (Failure "ambiguous")
  with Lookup.Exception _ ->
    raise Not_found

let classes_of_tags tags =
  let class_loader = Jutils.get_class_loader () in
  let set_of_tags =
    List.fold_left
      (fun acc elem -> StringSet.add elem acc)
      StringSet.empty
      tags in
  tags
  |> List.map
      (fun tag ->
        let result =
          Lookup.for_class
            false
            ~open_packages:[] (* because fully-qualified name are used in tags *)
            class_loader
            (UTF8.of_string (use_dots tag)) in
        let def = result.Lookup.value in
        add_all_parents class_loader def (StringSet.singleton tag)
        |> StringSet.remove tag)
  |> List.fold_left StringSet.union StringSet.empty
  |> StringSet.diff set_of_tags
  |> StringSet.elements
  |> List.map use_dots

(* Conversion from literal format-strings to typing information *)

type identifier = int

type conversion_function = (Types.type_desc -> Types.type_expr) -> string -> Location.t -> Types.type_expr * identifier

let rec ocaml_type_of_java_type ?(ellipsis=false) newty closed (desc : Descriptor.java_type) =
  match desc with
  | `Boolean        -> Predef.type_bool
  | `Byte           -> Predef.type_int
  | `Char           -> Predef.type_int
  | `Double         -> Predef.type_float
  | `Float          -> Predef.type_float
  | `Int            -> Predef.type_int32
  | `Long           -> Predef.type_int64
  | `Short          -> Predef.type_int
  | `Void           -> Predef.type_unit
  | `Array `Boolean -> (if ellipsis then Predef.type_array else Predef.type_java_boolean_array) Predef.type_bool
  | `Array `Byte    -> (if ellipsis then Predef.type_array else Predef.type_java_byte_array)    Predef.type_int
  | `Array `Char    -> (if ellipsis then Predef.type_array else Predef.type_java_char_array)    Predef.type_int
  | `Array `Double  -> (if ellipsis then Predef.type_array else Predef.type_java_double_array)  Predef.type_float
  | `Array `Float   -> (if ellipsis then Predef.type_array else Predef.type_java_float_array)   Predef.type_float
  | `Array `Int     -> (if ellipsis then Predef.type_array else Predef.type_java_int_array)     Predef.type_int32
  | `Array `Long    -> (if ellipsis then Predef.type_array else Predef.type_java_long_array)    Predef.type_int64
  | `Array `Short   -> (if ellipsis then Predef.type_array else Predef.type_java_short_array)   Predef.type_int
  | `Array at ->
      if ellipsis then
        (at :> Descriptor.java_type)
        |> ocaml_type_of_java_type newty closed
        |> Predef.type_array
      else
        (at :> Descriptor.java_type)
        |> ocaml_type_of_java_type newty closed
        |> Predef.type_java_reference_array
  | `Class cn ->
      let open Types in
      let class_loader = Jutils.get_class_loader () in
      let cn = Name.external_utf8_for_class cn in
      let cd = ClassLoader.find_class class_loader cn in
      let classes =
        add_all_parents class_loader cd StringSet.empty
        |> StringSet.elements in
      let type_desc =
        let row_desc = {
          row_fields = (List.map (fun s -> s, Rpresent None) classes);
          row_more = newty (Tvar None);
          row_bound = ();
          row_closed = closed;
          row_fixed = false;
          row_name = None;
        } in
        Tvariant row_desc in
      Tconstr (Predef.path_java_instance, [ newty type_desc ], ref Mnil)
      |> newty

let ocaml_type_of_java_type_list ?(ellipsis=false) newty closed l =
  List.mapi
    (fun i x ->
      let ellipsis = ellipsis && (i = pred @@ List.length l) in
      ocaml_type_of_java_type ~ellipsis newty closed x)
    (l :> Descriptor.java_type list)

let make_container conversion_function =
  let container = Hashtbl.create 17 in
  let length () = Hashtbl.length container in
  let add x y = Hashtbl.add container x y in
  let get id = Hashtbl.find container id in
  (* note: do not pass loader to conversion_function through partial
           application, because the loader should be lazily initialized. *)
  let convert = conversion_function length add in
  get, convert

let rec mk_arrow newty = function
  | hd :: [] -> hd
  | hd :: tl -> newty (Types.Tarrow ("", hd, mk_arrow newty tl, Types.Cok))
  | _ -> assert false

(* Constructors *)

type constructor_info = {
    constructor_class : ClassDefinition.t;
    constructor_method : Method.constructor;
    constructor_ellipsis : bool;
  }

let get_constructor_info, java_constructor_of_string =
  make_container (fun length add newty s loc ->
    let loader = Jutils.get_class_loader () in
    let result =
      try
        s
        |> use_dots
        |> UTF8.of_string
        |> Lookup.for_constructor
            false
            ~open_packages:(get_opened_packages loc)
            loader
      with Lookup.Exception e ->
        failwith (Lookup.string_of_error e) in
    let cd, cstr = result.Lookup.value in
    check_public_class loc cd;
    if not (AccessFlag.mem_constructor `Public cstr.Method.cstr_flags) then
      failwith "constructor is not public";
    check_deprecated loc (cstr.Method.cstr_attributes :> Attribute.t list);
    let ellipsis =
      (contains_dots s)
        && (AccessFlag.mem_constructor `Varargs cstr.Method.cstr_flags) in
    let id = length () in
    add id { constructor_class = cd; constructor_method = cstr; constructor_ellipsis = ellipsis };
    let params =
      cstr.Method.cstr_descriptor
      |> ocaml_type_of_java_type_list ~ellipsis newty false in
    let return =
      (`Class cd.ClassDefinition.name)
      |> ocaml_type_of_java_type newty true in
    let params = if params = [] then [ Predef.type_unit ] else params in
    newty (Types.Tconstr (Predef.path_java_constructor,
                          [mk_arrow newty (params @ [return])],
                          ref Types.Mnil)),
    id)

(* Arrays *)

(* take a string of the form ["abc[][]...[]"] and returns a couple with
  ["abc"] and the number of dimensions. *)
let split_array_shape s =
  let i = ref (pred (String.length s)) in
  let dims = ref 0 in
  while ((!i - 1) >= 0) && (s.[!i] = ']') && (s.[!i - 1] = '[') do
    i := !i - 2;
    incr dims
  done;
  String.sub s 0 (succ !i), !dims

(* take a string of the form ["abc[][]...[][_][_]...[_]"] and returns a
   triple with ["abc"], the total number of dimensions (i.e. with and
   without underscore), and the number of dimensions with underscore. *)
let split_array_shape_dims s =
  let i = ref (pred (String.length s)) in
  let dims = ref 0 in
  while ((!i - 1) >= 0) && (s.[!i] = ']') && (s.[!i - 1] = '[') do
    i := !i - 2;
    incr dims
  done;
  let dims2 = ref 0 in
  while ((!i - 2) >= 0) && (s.[!i] = ']') && (s.[!i - 1] = '_') && (s.[!i - 2] = '[') do
    i := !i - 3;
    incr dims2
  done;
  String.sub s 0 (succ !i), !dims + !dims2, !dims2

let rec make_array ty dims =
  if dims <= 0 then
    ty
  else
    `Array (make_array ty (pred dims))

type array_info = {
    array_type : Descriptor.array_type;
    array_total_dimensions : int;
    array_init_dimensions : int;
  }

let get_array_info, java_array_shape_of_string =
  make_container (fun length add specify_dimensions newty s loc ->
    let loader = Jutils.get_class_loader () in
    let s, array_total_dimensions, array_init_dimensions =
      if specify_dimensions then
        split_array_shape_dims s
      else
        let s, dims = split_array_shape s in
        s, dims, dims in
    if array_total_dimensions = 0 then raise (Failure "not an array descriptor");
    if array_init_dimensions = 0 then raise (Failure "invalid array descriptor");
    let base =
      match s with
      | "boolean" -> `Boolean
      | "byte"    -> `Byte
      | "char"    -> `Char
      | "double"  -> `Double
      | "float"   -> `Float
      | "int"     -> `Int
      | "long"    -> `Long
      | "short"   -> `Short
      | _ ->
          let result =
            try
              s
              |> use_dots
              |> UTF8.of_string
              |> Lookup.for_class
                false
                ~open_packages:(get_opened_packages loc)
                loader
          with Lookup.Exception e ->
            failwith (Lookup.string_of_error e) in
          let cd = result.Lookup.value in
          check_public_class loc cd;
          `Class cd.ClassDefinition.name in
    let array_type = `Array (make_array base (pred array_total_dimensions)) in
    let id = length () in
    add id { array_type; array_total_dimensions; array_init_dimensions };
    let rec arrow n ty =
      if n <= 0 then
        ty
      else
        newty (Types.Tarrow ("", Predef.type_int32, arrow (pred n) ty, Types.Cok)) in
    let path =
      if specify_dimensions then
        Predef.path_java_array_shape_dims
      else
        Predef.path_java_array_shape in
    let actual_dims =
      if specify_dimensions then
        array_init_dimensions
      else
        array_total_dimensions in
    newty (Types.Tconstr (path,
                          [arrow actual_dims
                             (ocaml_type_of_java_type newty true (array_type :> Descriptor.java_type))],
                          ref Types.Mnil)),
    id)

(* Methods *)

type method_info = {
    method_class : ClassDefinition.t;
    method_method : Method.regular;
    method_ellipsis : bool;
  }

let add_implicit_type s =
  if String.contains s ':' then
    s
  else
    s ^ ":_"

let get_method_info, java_method_of_string =
  make_container (fun length add newty s loc ->
    let loader = Jutils.get_class_loader () in
    let s = add_implicit_type s in
    let result =
      try
        s
        |> use_dots
        |> UTF8.of_string
        |> Lookup.for_regular_method
            false
            ~open_packages:(get_opened_packages loc)
            loader
      with Lookup.Exception e ->
        failwith (Lookup.string_of_error e) in
    let cd, meth = result.Lookup.value in
    check_public_class loc cd;
    if not (AccessFlag.mem_method `Public meth.Method.flags) then
      failwith "method is not public";
    check_deprecated loc (meth.Method.attributes :> Attribute.t list);
    let ellipsis =
      (contains_dots s)
        && (AccessFlag.mem_method `Varargs meth.Method.flags) in
    let id = length () in
    add id { method_class = cd; method_method = meth; method_ellipsis = ellipsis };
    let is_static = AccessFlag.mem_method `Static meth.Method.flags in
    let params, return = meth.Method.descriptor in
    let params =
      if is_static then
        params
      else
        (`Class cd.ClassDefinition.name) :: params in
    let params = ocaml_type_of_java_type_list ~ellipsis newty false params in
    let return = ocaml_type_of_java_type newty true return in
    let params =
      if params = [] then
        [ Predef.type_unit ]
      else
        params in
    newty (Types.Tconstr (Predef.path_java_method,
                          [mk_arrow newty (params @ [return])],
                          ref Types.Mnil)),
    id)

(* Fields *)

type field_info = {
    field_class : ClassDefinition.t;
    field_field : Field.t;
  }

let get_field_get_info, java_field_get_of_string =
  make_container (fun length add newty s loc ->
    let loader = Jutils.get_class_loader () in
    let s = add_implicit_type s in
    let result =
      try
        s
        |> use_dots
        |> UTF8.of_string
        |> Lookup.for_field
            false
            ~open_packages:(get_opened_packages loc)
            loader
      with Lookup.Exception e ->
        failwith (Lookup.string_of_error e) in
    let cd, field = result.Lookup.value in
    check_public_class loc cd;
    if not (AccessFlag.mem_field `Public field.Field.flags) then
      failwith "field is not public";
    check_deprecated loc (field.Field.attributes :> Attribute.t list);
    let id = length () in
    add id { field_class = cd; field_field = field };
    let is_static = AccessFlag.mem_field `Static field.Field.flags in
    let params =
      if is_static then
        [ Predef.type_unit ]
      else
        [ ocaml_type_of_java_type newty false (`Class cd.ClassDefinition.name) ] in
    let return = ocaml_type_of_java_type newty true (field.Field.descriptor :> Descriptor.java_type) in
    newty (Types.Tconstr (Predef.path_java_field_get,
                          [mk_arrow newty (params @ [return])],
                          ref Types.Mnil)),
    id)

let get_field_set_info, java_field_set_of_string =
  make_container (fun length add newty s loc ->
    let loader = Jutils.get_class_loader () in
    let s = add_implicit_type s in
    let result =
      try
        s
        |> use_dots
        |> UTF8.of_string
        |> Lookup.for_field
            false
            ~open_packages:(get_opened_packages loc)
            loader
      with Lookup.Exception e ->
        failwith (Lookup.string_of_error e) in
    let cd, field = result.Lookup.value in
    check_public_class loc cd;
    if not (AccessFlag.mem_field `Public field.Field.flags) then
      failwith "field is not public";
    if AccessFlag.mem_field `Final field.Field.flags then
      failwith "field is final";
    check_deprecated loc (field.Field.attributes :> Attribute.t list);
    let id = length () in
    add id { field_class = cd; field_field = field };
    let is_static = AccessFlag.mem_field `Static field.Field.flags in
    let params =
      if is_static then
        [ field.Field.descriptor ]
      else
        [ `Class cd.ClassDefinition.name ; field.Field.descriptor ] in
    let params = ocaml_type_of_java_type_list newty false params in
    let return = Predef.type_unit in
    newty (Types.Tconstr (Predef.path_java_field_set,
                          [mk_arrow newty (params @ [return])],
                          ref Types.Mnil)),
    id)

(* Dynamic type checks *)

type type_info = {
    type_class : BaristaLibrary.Descriptor.non_void_java_type;
  }

let get_reference_type_info, java_reference_type_of_string =
  make_container (fun length add newty s loc ->
    let loader = Jutils.get_class_loader () in
    let s, dims = split_array_shape s in
    let base =
      match s with
      | "boolean" when dims > 0 -> `Boolean
      | "byte" when dims > 0 -> `Byte
      | "char" when dims > 0 -> `Char
      | "double" when dims > 0 -> `Double
      | "float" when dims > 0 -> `Float
      | "int" when dims > 0 -> `Int
      | "long" when dims > 0 -> `Long
      | "short" when dims > 0 -> `Short
      | _ ->
          let result =
            try
              s
              |> use_dots
              |> UTF8.of_string
              |> Lookup.for_class
                  false
                  ~open_packages:(get_opened_packages loc)
                  loader
            with Lookup.Exception e ->
              failwith (Lookup.string_of_error e) in
          let cd = result.Lookup.value in
          check_public_class loc cd;
          `Class cd.ClassDefinition.name in
    let shape =
      if dims > 0 then
        `Array (make_array base (pred dims))
      else
        base in
    let id = length () in
    add id { type_class = shape };
    newty (Types.Tconstr (Predef.path_java_reference_type,
                          [ocaml_type_of_java_type newty true shape],
                          ref Types.Mnil)),
    id)

(* Type information} *)

type any_type_info = {
    any_type_desc : BaristaLibrary.Descriptor.java_type;
  }

let get_any_type_info, java_any_type_of_string =
  make_container (fun length add newty s loc ->
    let loader = Jutils.get_class_loader () in
    let s, dims = split_array_shape s in
    let typ =
      match s with
      | "boolean" -> if dims = 0 then `Boolean else `Array (make_array `Boolean (pred dims))
      | "byte"    -> if dims = 0 then `Byte    else `Array (make_array `Byte    (pred dims))
      | "char"    -> if dims = 0 then `Char    else `Array (make_array `Char    (pred dims))
      | "double"  -> if dims = 0 then `Double  else `Array (make_array `Double  (pred dims))
      | "float"   -> if dims = 0 then `Float   else `Array (make_array `Float   (pred dims))
      | "int"     -> if dims = 0 then `Int     else `Array (make_array `Int     (pred dims))
      | "long"    -> if dims = 0 then `Long    else `Array (make_array `Long    (pred dims))
      | "short"   -> if dims = 0 then `Short   else `Array (make_array `Short   (pred dims))
      | "void"    -> if dims = 0 then `Void    else failwith "void cannot be used as an array element type"
      | _ ->
          let result =
            try
              s
              |> use_dots
              |> UTF8.of_string
              |> Lookup.for_class
                  false
                  ~open_packages:(get_opened_packages loc)
                  loader
            with Lookup.Exception e ->
              failwith (Lookup.string_of_error e) in
          let cd = result.Lookup.value in
          check_public_class loc cd;
          let base = `Class cd.ClassDefinition.name in
          if dims > 0 then
            `Array (make_array base (pred dims))
          else
            base in
    let id = length () in
    add id { any_type_desc = typ };
    newty (Types.Tconstr (Predef.path_java_any_type,
                          [ocaml_type_of_java_type newty true typ],
                          ref Types.Mnil)),
    id)

(* Proxies *)

type proxy_info = {
    proxy_class : Name.for_class;
    proxy_classes : ClassDefinition.t list;
    proxy_mapping : (string * string * BaristaLibrary.Descriptor.for_method * string) list;
  }

module ClassNameSet = Set.Make (struct
  type t = ClassDefinition.t
  let compare x y =
    let x = x.ClassDefinition.name in
    let y = y.ClassDefinition.name in
    Name.compare_for_class x y
end)

module IntSet = Set.Make (struct
  type t = int
  let compare (x : int) (y : int) = Pervasives.compare x y
end)

let split seps s =
  let idx = ref 0 in
  let len = String.length s in
  let buff = Buffer.create len in
  let res = ref [] in
  let in_sep = ref false in
  while !idx < len do
    if !in_sep then begin
      if not (String.contains seps s.[!idx]) then begin
        Buffer.add_char buff s.[!idx];
        in_sep := false
      end
    end else begin
      if String.contains seps s.[!idx] then begin
        res := (Buffer.contents buff) :: !res;
        Buffer.clear buff;
        in_sep := true
      end else
        Buffer.add_char buff s.[!idx]
    end;
    incr idx
  done;
  let last = Buffer.contents buff in
  if last <> "" then res := last :: !res;
  List.rev !res


let java_lang_Object =
  "java.lang.Object"
  |> UTF8.of_string
  |> Name.make_for_class_from_external

let java_lang_String =
  "java.lang.String"
  |> UTF8.of_string
  |> Name.make_for_class_from_external

exception Already_defined

type name_mapping = {
    nm_class_name : string;
    nm_meth_name : string;
    nm_meth_desc : Descriptor.for_method;
    nm_ocaml_name : string;
  }

let utf8_of_regular { Method.name; descriptor = (params, return); flags = _; attributes = _ } =
  let (++) = UTF8.(++) in
  (Name.utf8_for_method name)
    ++ (UTF8.of_string "(")
    ++ (UTF8.concat_sep_map
          (UTF8.of_string ", ")
          Descriptor.external_utf8_of_java_type
          (params :> Descriptor.java_type list))
    ++ (UTF8.of_string "):")
    ++ (Descriptor.external_utf8_of_java_type return)

let to_string_desc = ([], `Class java_lang_String)

let equals_desc = ([`Class java_lang_Object], `Boolean)

let hash_code_desc = ([], `Int)

let compute_mapping loader classes additional_methods newty =
  let mapping : name_mapping list ref = ref [] in
  let tags = ref IntSet.empty in
  let add_method_name cn mn (md : Descriptor.for_method) =
    let already_defined =
      List.exists
        (fun { nm_meth_name; nm_meth_desc; nm_class_name = _; nm_ocaml_name = _ } ->
          (mn = nm_meth_name) && (Descriptor.equal_for_method md nm_meth_desc))
        !mapping in
    let tag = Btype.hash_variant mn in
    if already_defined then begin
      (* the method has already been defined (e.g. by a parent),
         store the mapping but do not add a new OCaml method *)
      mapping := { nm_class_name = cn;
                   nm_meth_name = mn;
                   nm_meth_desc = md;
                   nm_ocaml_name = mn; } :: !mapping;
      raise Already_defined
    end else if IntSet.mem tag !tags then begin
      (* the method tag would clash with a previously-defined one,
         hence the need to rename it *)
      let new_tag = ref tag in
      let new_name = ref mn in
      let i = ref 2 in
      while IntSet.mem !new_tag !tags do
        new_name := Printf.sprintf "%s_%d" mn !i;
        new_tag := Btype.hash_variant !new_name;
        incr i
      done;
      mapping := { nm_class_name = cn;
                   nm_meth_name = mn;
                   nm_meth_desc = md;
                   nm_ocaml_name = !new_name; } :: !mapping;
      tags := IntSet.add !new_tag !tags;
      !new_name
    end else begin
      (* the method has not been previously defined, and its tag does
         not clash with an existing one *)
      mapping := { nm_class_name = cn;
                   nm_meth_name = mn;
                   nm_meth_desc = md;
                   nm_ocaml_name = mn; } :: !mapping;
      tags := IntSet.add tag !tags;
      mn
    end in
  let classes_with_parents =
    classes
    |> List.map (fun x -> interface_parents loader x)
    |> List.flatten
    |> List.fold_left
        (fun acc elem -> ClassNameSet.add elem acc)
        ClassNameSet.empty
    |> ClassNameSet.elements in
  let overloaded_methods = ref [] in
  let methods =
    classes_with_parents
    |> List.map
        (fun def ->
          let def_name =
            def.ClassDefinition.name
            |> Name.external_utf8_for_class 
            |> UTF8.to_string in
          List.fold_right
            (fun (elem : Method.t) (acc : Method.regular list) ->
              match elem with
              | Method.Regular reg
                when (AccessFlag.mem_method `Public reg.Method.flags)
                    && (not (AccessFlag.mem_method `Static reg.Method.flags))
                    && (AccessFlag.mem_method `Abstract reg.Method.flags) ->
                  reg :: acc
              | Method.Regular { Method.flags; name; descriptor; attributes = _ }
                  when AccessFlag.mem_method `Public flags ->
                  let name = UTF8.to_string_noerr (Name.utf8_for_method name) in
                  begin match name with
                  | "toString" when Descriptor.equal_for_method descriptor to_string_desc ->
                      overloaded_methods := name :: !overloaded_methods
                  | "equals"   when Descriptor.equal_for_method descriptor equals_desc ->
                      overloaded_methods := name :: !overloaded_methods
                  | "hashCode" when Descriptor.equal_for_method descriptor hash_code_desc ->
                      overloaded_methods := name :: !overloaded_methods
                  | _ -> ();
                  end;
                  acc
              | Method.Regular _ | Method.Constructor _ | Method.Initializer _ ->
                  acc)
            def.ClassDefinition.methods
            []
          |> List.map (fun x -> utf8_of_regular x, def_name, x))
    |> (fun x ->
        x
        @ [(additional_methods @ !overloaded_methods)
           |> List.map
               (fun name ->
                 let make_regular descriptor =
                   let reg =
                     { Method.flags = [`Public];
                       name = Name.make_for_method (UTF8.of_string name);
                       descriptor;
                       attributes = []; } in
                   utf8_of_regular reg, "java.lang.Object", reg in
                 match name with
                 | "toString" -> make_regular to_string_desc
                 | "equals"   -> make_regular equals_desc
                 | "hashCode" -> make_regular hash_code_desc
                 | _          -> assert false)])
    |> List.flatten
    |> List.sort (fun (x, _, _) (y, _, _) -> UTF8.compare x y)
    |> List.map (fun (_, x, y) -> x, y)
    |> List.fold_left
        (fun acc (def, meth) ->
          try
            let meth_name =
              meth.Method.name
              |> Name.utf8_for_method
              |> UTF8.to_string in
            let name = add_method_name def meth_name meth.Method.descriptor in
            let params, return = meth.Method.descriptor in
            let params = ocaml_type_of_java_type_list newty false params in
            let return = ocaml_type_of_java_type newty true return in
            let ty = mk_arrow newty (params @ [return]) in
            let ty = newty (Types.Tpoly (ty, [])) in
            newty (Types.Tfield (name, Types.Fpresent, ty, acc))
          with Already_defined ->
            acc)
        (newty (Types.Tvar None)) in
  let mapping =
    List.map
      (fun { nm_class_name; nm_meth_name; nm_meth_desc; nm_ocaml_name } ->
        nm_class_name, nm_meth_name, nm_meth_desc, nm_ocaml_name)
      !mapping in
  methods, mapping

let get_proxy_info, java_proxy_of_string =
  make_container (fun length add newobj newty sl loc ->
    let loader = Jutils.get_class_loader () in
    let defs, obj_meths =
      sl
      |> split ","
      |> List.fold_left
          (fun (acc_defs, acc_obj_methds) elem ->
            match String.trim elem with
            | (".toString" | ".equals" | ".hashCode") as name ->
                acc_defs, (String.sub name 1 (pred (String.length name))) :: acc_obj_methds
            | cn ->
                (try
                  cn
                  |> use_dots
                  |> UTF8.of_string
                  |> Lookup.for_class
                      false
                      ~open_packages:(get_opened_packages loc)
                      loader
                  |> (fun x -> x.Lookup.value)
                with Lookup.Exception e ->
                  failwith (Lookup.string_of_error e)) :: acc_defs,
                acc_obj_methds)
          ([], []) in
    let classes =
      defs
      |> List.fold_left
        (fun acc_classes def ->
          let is_interface =
            AccessFlag.mem_class `Interface def.ClassDefinition.access_flags in
          if not is_interface then
            failwith "type is not an interface";
          check_public_class loc def;
          check_deprecated loc (def.ClassDefinition.attributes :> Attribute.t list);
          if ClassNameSet.mem def acc_classes then
            failwith "duplicate inferface";
          ClassNameSet.add def acc_classes)
        ClassNameSet.empty
      |> ClassNameSet.elements in
    if classes = [] then
      failwith "no interface provided";
    let methods, mapping = compute_mapping loader classes obj_meths newty in
    let return =
      match classes with
      | [ x ] -> x.ClassDefinition.name
      | _ -> java_lang_Object in
    let id = length () in
    add id { proxy_class = return; proxy_classes = classes; proxy_mapping = mapping; };
    let return = ocaml_type_of_java_type newty true (`Class return) in
    newty (Types.Tconstr (Predef.path_java_proxy,
                          [mk_arrow newty [newobj methods; return]],
                          ref Types.Mnil)),
    id)

(* Miscellaneous *)

let get_arity prim_name id =
  let atleast1 x =
    if x < 1 then 1 else x in
  match prim_name with
  | "java constructor" ->
      let cstr_info = get_constructor_info id in
      cstr_info.constructor_method.Method.cstr_descriptor
      |> List.length
      |> atleast1
  | "java make array" | "java make array dims" ->
      let array_info = get_array_info id in
      array_info.array_init_dimensions
  | "java method call" ->
      let meth_info = get_method_info id in
      let is_static = AccessFlag.mem_method `Static meth_info.method_method.Method.flags in
      let nb_params =
        meth_info.method_method.Method.descriptor
        |> fst
        |> List.length in
      atleast1 (if is_static then nb_params else succ nb_params)
  | "java field get" ->
      1
  | "java field set" ->
      let field_info = get_field_set_info id in
      let is_static = AccessFlag.mem_field `Static field_info.field_field.Field.flags in
      if is_static then 1 else 2
  | "java instanceof" ->
      1
  | "java cast" ->
      1
  | "java class" ->
      1
  | "java proxy loader" ->
      2
  | "java proxy system" ->
      1
  | "java proxy runtime" ->
      1
  | _ ->
      Misc.fatal_error "Jtypes.get_arity"
