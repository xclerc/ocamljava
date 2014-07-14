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


type library_init =
  | Explicit
  | Static

let library_inits = [
  "explicit",  Explicit ;
  "static",    Static ;
]

let library_inits_symbols = List.map fst library_inits

type string_mapping =
  | Java_string
  | OCamlString
  | Byte_array

let string_mappings = [
  "java-string",  Java_string ;
  "ocamlstring",  OCamlString ;
  "byte-array",   Byte_array ;
]

let string_mappings_symbols = List.map fst string_mappings

type file = {
    file_name : string;
    file_package : string option;
  }

let class_name_prefix = ref ""

let class_name_suffix = ref "Wrapper"

let includes = ref []

let add_include s =
  includes := s :: !includes

let library_args = ref None

let library_init = ref Explicit

let library_package = ref None

let no_warnings = ref false

let package = ref None

let string_mapping = ref Java_string

let verbose = ref false

let files = ref []

let add_file x =
  let file =
    try
      let idx_at = String.index x '@' in
      let file_name = String.sub x 0 idx_at in
      let file_package = Some (String.sub x (succ idx_at) ((String.length x) - idx_at - 1)) in
      { file_name; file_package }
    with Not_found ->
    { file_name = x; file_package = None } in
  files := file :: !files

let switches = [
  "-class-name-prefix",
  Arg.Set_string class_name_prefix,
  "<string>  Set prefix for class names";

  "-class-name-suffix",
  Arg.Set_string class_name_suffix,
  "<string>  Set suffix for class names";

  "-I",
  Arg.String add_include,
  "<string>  Add to search path";

  "-library-args",
  Arg.String (fun s -> library_args := Some s),
  "<string>  Arguments passed for library initialization";

  "-library-init",
  Arg.Symbol (library_inits_symbols,
              (fun s -> library_init := List.assoc s library_inits)),
  "  Set initialization mode";

  "-library-package",
  Arg.String (fun s -> library_package := Some s),
  "<string>  Set library package";

  "-no-warnings",
  Arg.Set no_warnings,
  " Disable warnings" ;

  "-package",
  Arg.String (fun s -> package := Some s),
  "<string>  Set package name";

  "-string-mapping",
  Arg.Symbol (string_mappings_symbols,
             (fun s -> string_mapping := List.assoc s string_mappings)),
  "  Set mapping for strings";

  "-verbose",
  Arg.Set verbose,
  " Enable verbose mode" ;
]

let usage =
  Printf.sprintf "Usage: %s <options> <files>\nOptions are:"
    (Filename.basename Sys.argv.(0))

let parse () =
  Arg.parse switches add_file usage;
  match !library_init, !library_package with
  | Static, None ->
      Wrap_common.(fail (Command_line_inconsistency "no library package name"))
  | Explicit, Some _ ->
      Wrap_common.(fail (Command_line_inconsistency "library package name is not used"))
  | _ ->
      ()

let class_of_module modname =
  Printf.sprintf "%s%s%s"
    !class_name_prefix
    modname
    !class_name_suffix
