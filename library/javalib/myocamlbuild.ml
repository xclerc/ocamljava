(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocamlbuild_plugin

let odocl_file = Pathname.pwd / "javalib.odocl"
let mllib_file = Pathname.pwd / "javalib.mllib"
let src_path = Pathname.pwd / "src"
let excluded_modules = []

type array_module = {
    module_name : string;
    java_element_type : string;
    ocaml_element_type : string;
    ocaml_java_type : string;
    zero : string;
    zero_value : string;
    extra_intf : string list;
    extra_impl : string list;
  }

let subst_of_array_module am = function
  | "module_name" -> am.module_name
  | "java_element_type" -> am.java_element_type
  | "ocaml_element_type" -> am.ocaml_element_type
  | "ocaml_java_type" -> am.ocaml_java_type
  | "zero" -> am.zero
  | "zero_value" -> am.zero_value
  | "extra_intf" -> if am.extra_intf = [] then "" else ("\n" ^ (String.concat "\n" am.extra_intf) ^ "\n")
  | "extra_impl" -> if am.extra_impl = [] then "" else ("\n" ^ (String.concat "\n" am.extra_impl) ^ "\n")
  | _ -> assert false

let array_modules = [
  { module_name = "JavaBooleanArray";
    java_element_type = "boolean";
    ocaml_element_type = "bool";
    ocaml_java_type = "java_boolean_array";
    zero = "[false]";
    zero_value = "false";
    extra_intf = [];
    extra_impl = []; } ;
  { module_name = "JavaByteArray";
    java_element_type = "byte";
    ocaml_element_type = "int";
    ocaml_java_type = "java_byte_array";
    zero = "zero";
    zero_value = "0";
    extra_intf = [
      "external to_string : e java_byte_array -> string =" ;
      "  \"string_of_byte_array\"" ;
      "(** [to_string x] returns the passed array as wrapped into a string. *)" ;
      "" ;
      "external of_string : string -> e java_byte_array =" ;
      "  \"byte_array_of_string\"" ;
      "(** [of_string x] returns the array wrapped inside the passed string. *)" ;
    ];
    extra_impl = [
      "external to_string : e java_byte_array -> string =" ;
      "  \"string_of_byte_array\"" ;
      "" ;
      "external of_string : string -> e java_byte_array =" ;
      "  \"byte_array_of_string\"" ;
    ]; } ;
  { module_name = "JavaCharArray";
    java_element_type = "char";
    ocaml_element_type = "int";
    ocaml_java_type = "java_char_array";
    zero = "zero";
    zero_value = "0";
    extra_intf = [];
    extra_impl = []; } ;
  { module_name = "JavaDoubleArray";
    java_element_type = "double";
    ocaml_element_type = "float";
    ocaml_java_type = "java_double_array";
    zero = "zero";
    zero_value = "0.";
    extra_intf = [];
    extra_impl = []; } ;
  { module_name = "JavaFloatArray";
    java_element_type = "float";
    ocaml_element_type = "float";
    ocaml_java_type = "java_float_array";
    zero = "zero";
    zero_value = "0.";
    extra_intf = [];
    extra_impl = []; } ;
  { module_name = "JavaIntArray";
    java_element_type = "int";
    ocaml_element_type = "int32";
    ocaml_java_type = "java_int_array";
    zero = "zero";
    zero_value = "0l";
    extra_intf = [];
    extra_impl = []; } ;
  { module_name = "JavaLongArray";
    java_element_type = "long";
    ocaml_element_type = "int64";
    ocaml_java_type = "java_long_array";
    zero = "zero";
    zero_value = "0L";
    extra_intf = [];
    extra_impl = []; } ;
  { module_name = "JavaShortArray";
    java_element_type = "short";
    ocaml_element_type = "int";
    ocaml_java_type = "java_short_array";
    zero = "zero";
    zero_value = "0";
    extra_intf = [];
    extra_impl = []; }
]

let () =
  let odocl_chan = open_out odocl_file in
  let mllib_chan = open_out mllib_file in
  List.iter
    (fun { module_name; _ } ->
      output_string odocl_chan module_name;
      output_char odocl_chan '\n';
      output_string mllib_chan module_name;
      output_char mllib_chan '\n')
    array_modules;
  let add_file filename =
    if (Pathname.check_extension filename "mli")
      || (Pathname.check_extension filename "mly")
      || (Pathname.check_extension filename "mll") then begin
          let modulename = Pathname.remove_extension filename in
          let modulename = Pathname.basename modulename in
          let modulename = String.capitalize modulename in
          if not (List.mem modulename excluded_modules) then begin
            output_string odocl_chan modulename;
            output_char odocl_chan '\n';
            output_string mllib_chan modulename;
            output_char mllib_chan '\n'
          end
      end in
  Array.iter
    (fun path ->
      let path = src_path / path in
      if Pathname.is_directory path then
        Array.iter add_file (Pathname.readdir path)
      else
        add_file path)
    (Pathname.readdir src_path);
  close_out_noerr odocl_chan;
  close_out_noerr mllib_chan

let () =
  let safe_cp src dst =
    let src = Pathname.mk src in
    let dst = Pathname.mk dst in
    let dir = Pathname.dirname dst in
    let cmd = Printf.sprintf "mkdir -p %s" (Pathname.to_string dir) in
    if Sys.command cmd <> 0 then failwith ("cannot run " ^ cmd);
    cp src dst in
  dispatch begin function
    | After_rules ->
        flag ["ocaml"; "compile"; "warnings"] (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        flag ["ocaml"; "doc"] (A"-sort");
        flag ["ocaml"; "doc"; "servlet-api"]
          (S[A"-cp"; A"../../../external-jars/servlet-api.jar"]);
        flag ["ocaml"; "compile"; "servlet-api"]
          (S[A"-cp"; A"../../../external-jars/servlet-api.jar"]);
        List.iter
          (fun am ->
            let apply_template in_chan out_chan subst =
              try
                while true do
                  let line = input_line in_chan in
                  let buff = Buffer.create ((String.length line) * 2) in
                  Buffer.add_substitute buff subst line;
                  output_string out_chan (Buffer.contents buff);
                  output_char out_chan '\n'
                done
              with End_of_file -> () in
            let make_rule suffix =
              let file_path =
                "src/" ^ (String.uncapitalize am.module_name) ^ suffix in
              rule file_path
                ~prod:file_path
                ~insert:`bottom
                (fun _ _ ->
                  let name, chan = Filename.open_temp_file "array" suffix in
                  let template = open_in ("../templates/array-template" ^ suffix) in
                  apply_template template chan (subst_of_array_module am);
                  close_out_noerr chan;
                  safe_cp name file_path) in
            make_rule ".ml";
            make_rule ".mli")
          array_modules;
    | _ -> ()
  end
