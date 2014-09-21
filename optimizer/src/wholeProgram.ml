(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java optimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java optimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open BaristaLibrary
open Utils


let shared_constants_class_name =
  (* value is changed when we encounter an entry point *)
  "pack.ocamljavaConstants"
  |> UTF8.of_string
  |> Name.make_for_class_from_external
  |> ref

module ClassNameMap = Map.Make (struct
  type t = Name.for_class
  let compare x y = Name.compare_for_class x y
end)

let constants_name_to_main_name : Name.for_class ClassNameMap.t ref =
  ref ClassNameMap.empty

let constant_fields : (Field.t list) ClassNameMap.t ref =
  ref ClassNameMap.empty

let is_constants_class cn =
  ClassNameMap.mem cn !constants_name_to_main_name

let main_class_of_constants_class cn =
  ClassNameMap.find cn !constants_name_to_main_name

let additional_fields cn =
  try
    ClassNameMap.find cn !constant_fields
  with Not_found ->
    []

class for_constants = object

  inherit ClassTraversal.default_class_definition_iterator

  method! class_definition
      (_ : AccessFlag.for_class list)
      (cn : Name.for_class)
      (_ : Name.for_class option)
      (_ : Name.for_class list)
      (fields : Field.t list)
      (_ : Method.t list)
      (_ : Attribute.for_class list) =
    let s = Name.internal_utf8_for_class cn in
    let len = UTF8.length s in
    let last_dollar = UTF8.rindex_from s (pred len) (UChar.of_char '$') in
    let s = UTF8.substring s 0 (pred last_dollar) in
    let cn' = Name.make_for_class_from_internal s in
    constants_name_to_main_name := ClassNameMap.add cn cn' !constants_name_to_main_name;
    constant_fields := ClassNameMap.add cn' fields !constant_fields

end

module IntSet = Set.Make (struct
  type t = int
  let compare (x : int) (y : int) = Pervasives.compare x y
end)

let extract_ints l =
  List.fold_left
    (fun acc elem ->
      match elem with
      | Annotation.Int_value x -> IntSet.add (Int32.to_int x) acc
      | _                      -> assert false)
    IntSet.empty
    l

let linked_classes : UTF8.t list option ref = ref None

type indices = {
    read_indices : IntSet.t;
    written_indices : IntSet.t;
  }

(* map from class names to lists of referenced globals as
   (referenced_class, referenced_indices) couples *)
let indices : (UTF8.t * indices) list ClassNameMap.t ref =
  ref ClassNameMap.empty

let merge_maps (dst : indices ClassNameMap.t) (name, uses) =
  let class_name = Name.make_for_class_from_external name in
  let value =
    try
      let old = ClassNameMap.find class_name dst in
      { read_indices    = IntSet.union old.read_indices uses.read_indices;
        written_indices = IntSet.union old.written_indices uses.written_indices; }
    with Not_found ->
      uses in
  ClassNameMap.add class_name value dst

(* returns a map from class names to list of used globals *)
let get_used_indices () =
  ClassNameMap.fold
    (fun user used acc ->
      let merge =
        match !linked_classes with
        | Some l ->
            List.exists
              (fun x ->
                UTF8.equal x (Name.external_utf8_for_class user))
              l
        | None -> false in
      if merge then begin
        List.fold_left merge_maps acc used
      end else
        acc)
    !indices
    ClassNameMap.empty

let make_remove_indices_function () =
  let used_indices = get_used_indices () in
  if !Args.verbose then begin
    used_indices
    |> ClassNameMap.iter
        (fun cn { read_indices; written_indices } ->
          cn
          |> Name.external_utf8_for_class
          |> UTF8.to_string
          |> Printf.printf "indices for %S:\n";
          IntSet.iter
            (Printf.printf "  - read %d\n")
            read_indices;
          IntSet.iter
            (Printf.printf "  - written %d\n")
            written_indices);
    Printf.printf "%!"
  end;
  fun name index ->
    try
      let indices = ClassNameMap.find name used_indices in
      (not (IntSet.mem index indices.read_indices)) &&
      (not (IntSet.mem (~-1) indices.read_indices))
    with Not_found ->
      false

class iterator zip = object

  inherit ArchiveTraversal.default_archive_iterator zip

  val for_constants = new for_constants

  method! class_definition cd =
    let annotations =
      Attribute.extract_annotations
        (cd.ClassDefinition.attributes :> Attribute.t list) in
    let global_uses = ref [] in
    List.iter
      (fun (n, l) ->
        if Name.equal_for_class n Names.global_uses then begin
          let class_name = ref (UTF8.of_string "dummy.Class") in
          let read_indices = ref IntSet.empty in
          let written_indices = ref IntSet.empty in
          List.iter
            (fun (n, v) ->
              let n = UTF8.to_string n in
              match n, v with
              | "className", Annotation.String_value x ->
                  class_name := x
              | "readIndices", Annotation.Array_value x ->
                  read_indices := extract_ints x
              | "writtenIndices", Annotation.Array_value x ->
                  written_indices := extract_ints x
              | _ -> ())
            l;
          global_uses :=
            (!class_name,
             { read_indices    = !read_indices;
               written_indices = !written_indices; }) :: !global_uses;
        end)
      annotations;
    indices := ClassNameMap.add cd.ClassDefinition.name !global_uses !indices;
    let is_constants =
      List.exists
        (fun (n, _) -> Name.equal_for_class n Names.constants_class)
        annotations in
    let is_entry_point =
      List.exists
        (fun (n, ev) ->
          if Name.equal_for_class n Names.entry_point then begin
            List.iter
              (fun (k, v) ->
                if UTF8.equal k Names.linked_classes then
                  match v with
                  | Annotation.Array_value l ->
                      let l =
                        List.map
                          (function
                            | Annotation.String_value x -> x
                            | _                         -> assert false)
                          l in
                      if !Args.verbose then begin
                        Printf.printf "linked classes:\n";
                        List.iter
                          (fun cn ->
                            Printf.printf "  - %S\n" (UTF8.to_string cn))
                          l;
                        Printf.printf "%!"
                      end;
                      linked_classes := Some l
                  | _ -> assert false)
              ev;
            true
          end else
            false)
        annotations in
    if is_constants then
      ClassDefinition.iter for_constants cd;
    if is_entry_point then begin
      let package_name, _ =
        Name.split_class_name cd.ClassDefinition.name in
      let class_name =
        "ocamljavaConstants"
        |> UTF8.of_string
        |> Name.make_for_class_from_external in
      shared_constants_class_name := Name.gather_class_name package_name class_name
    end

end

let iter_archive filename =
  let make_iterator x = new iterator x in
  ArchiveTraversal.iter_file make_iterator filename

let shared_constants : UTF8.Set.t ref = ref UTF8.Set.empty

let add_shared_constant name =
  shared_constants := UTF8.Set.add (Name.utf8_for_field name) !shared_constants

let empty_cstr =
  let code =
    Attribute.({ max_stack = u2 1;
                 max_locals = u2 1;
                 code =
                   [ Instruction.ALOAD_0 ;
                     Instruction.INVOKESPECIAL
                       (Names.object_,
                        Misc.make_method_name "<init>",
                        ([], `Void)) ;
                     Instruction.RETURN ];
                 exception_table = [];
                 attributes = []; }) in
  Method.(Constructor { cstr_flags = [ `Private ];
                        cstr_descriptor = [];
                        cstr_attributes = [`Code code]; })

external identity : 'a -> 'a = "%identity"

let push_int32 = function
  | (-1l) -> Instruction.ICONST_M1
  | 0l    -> Instruction.ICONST_0
  | 1l    -> Instruction.ICONST_1
  | 2l    -> Instruction.ICONST_2
  | 3l    -> Instruction.ICONST_3
  | 4l    -> Instruction.ICONST_4
  | 5l    -> Instruction.ICONST_5
  | n when n >= (-128l) && n <= 127l     -> Instruction.BIPUSH (s1 (Int32.to_int n))
  | n when n >= (-32768l) && n <= 32767l -> Instruction.SIPUSH (s2 (Int32.to_int n))
  | n     -> Instruction.LDC_W (`Int n)

let push_int64 = function
  | 0L -> [ Instruction.LCONST_0 ]
  | 1L -> [ Instruction.LCONST_1 ]
  | 2L -> [ Instruction.ICONST_2; Instruction.I2L ]
  | 3L -> [ Instruction.ICONST_3; Instruction.I2L ]
  | 4L -> [ Instruction.ICONST_4; Instruction.I2L ]
  | 5L -> [ Instruction.ICONST_5; Instruction.I2L ]
  | n  -> [ Instruction.LDC2_W (`Long n) ]

let push_double x =
  if x = 0.0 then Instruction.DCONST_0
  else if x = 1.0 then Instruction.DCONST_1
  else Instruction.LDC2_W (`Double x)

let compile_shared_constant_class () =
  let fields_and_inits =
    List.map
      (fun name ->
        let underscore = UTF8.index_from name 0 (UChar.of_char '_') in
        let typ = UTF8.substring name 0 (pred underscore) in
        let hexa =
          UTF8.substring name (succ underscore) (pred (UTF8.length name))
          |> UTF8.to_string in
        let field_name = Name.make_for_field name in
        let field =
          Field.({ flags = [ `Public; `Static; `Final ];
                   name = field_name;
                   descriptor = `Class Names.value;
                   attributes = []; }) in
        let init =
          match UTF8.to_string_noerr typ with
          | "INT" ->
              (Scanf.sscanf hexa "%LX" identity |> push_int64)
              @ [ Instruction.INVOKESTATIC (Names.value,
                                            Misc.make_method_name "createLong",
                                            ([`Long], `Class Names.value)) ]
          | "INT32" ->
              [ Scanf.sscanf hexa "%lX" identity |> push_int32;
                Instruction.INVOKESTATIC (Names.value,
                                          Misc.make_method_name "createInt32",
                                          ([`Int], `Class Names.value)) ]
          | "INT64" ->
              (Scanf.sscanf hexa "%LX" identity |> push_int64)
              @ [ Instruction.INVOKESTATIC (Names.value,
                                            Misc.make_method_name "createInt64",
                                            ([`Long], `Class Names.value)) ]
          | "NATIVEINT" ->
              (Scanf.sscanf hexa "%nX" identity |> Int64.of_nativeint |> push_int64)
              @ [ Instruction.INVOKESTATIC (Names.value,
                                            Misc.make_method_name "createNativeInt",
                                            ([`Long], `Class Names.value)) ]
          | "FLOAT" ->
              [ Scanf.sscanf hexa "%LX" identity |> Int64.float_of_bits |> push_double ;
                Instruction.INVOKESTATIC (Names.value,
                                          Misc.make_method_name "createDouble",
                                          ([`Double], `Class Names.value)) ]

          | _ -> assert false  in
        let init =
          init
          @ [ Instruction.PUTSTATIC (!shared_constants_class_name,
                                     field_name,
                                     `Class Names.value) ] in
      field, init)
      (UTF8.Set.elements !shared_constants) in
  let fields, inits = List.split fields_and_inits in
  let initializer_meth =
    let code = Attribute.({ max_stack = u2 12;
                            max_locals = u2 0;
                            code = (List.flatten inits) @ [ Instruction.RETURN ];
                            exception_table = [];
                            attributes = [] }) in
    Method.(Initializer { init_flags = [`Static];
                          init_attributes = [`Code code]}) in
  let cd =
    ClassDefinition.({ access_flags = [ `Public; `Final; `Super ];
                       name = !shared_constants_class_name;
                       extends = Some Names.object_;
                       implements = [];
                       fields = fields;
                       methods = [ empty_cstr; initializer_meth ];
                       attributes = [] }) in
  let buff = ByteBuffer.make_of_size 2048 in
  let os = OutputStream.make_of_buffer buff in
  ClassFile.write (ClassDefinition.encode cd) os;
  OutputStream.close os;
  let contents = ByteBuffer.contents buff in
  let path = Name.internal_utf8_for_class !shared_constants_class_name in
  let path = UTF8.(path ++ (UTF8.of_string ".class")) in
  let path =
    if !Args.war then
      UTF8.((UTF8.of_string "WEB-INF/classes/") ++ path)
    else
      path in
  path, contents
