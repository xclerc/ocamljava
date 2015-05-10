(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2015 Xavier Clerc.
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
let src_path   = Pathname.pwd / "src"
let excluded_modules = []

type array_module = {
    array_module_name  : string;
    java_element_type  : string;
    ocaml_element_type : string;
    ocaml_java_type    : string;
    zero               : string;
    zero_value         : string;
    extra_intf         : string list;
    extra_impl         : string list;
  }

let array_module
    ~module_name
    ~java_element_type
    ?(ocaml_element_type = "")
    ?(ocaml_java_type = "")
    ?(zero = "zero")
    ~zero_value
    ?(extra_intf = [])
    ?(extra_impl = [])
    () =
  let ocaml_element_type =
    if ocaml_element_type = "" then
      "java_" ^ java_element_type
    else
      ocaml_element_type in
  let ocaml_java_type =
    if ocaml_java_type = "" then
      "java_" ^ java_element_type ^ "_array"
    else
      ocaml_java_type in
  { array_module_name = module_name;
    java_element_type;
    ocaml_element_type;
    ocaml_java_type;
    zero;
    zero_value;
    extra_intf;
    extra_impl }

let subst_of_array_module am name =
  let concat l =
    if l <> [] then
      "\n" ^ (String.concat "\n" l) ^ "\n"
    else
      "" in
  match name with
  | "module_name"        -> am.array_module_name
  | "java_element_type"  -> am.java_element_type
  | "ocaml_element_type" -> am.ocaml_element_type
  | "ocaml_java_type"    -> am.ocaml_java_type
  | "zero"               -> am.zero
  | "zero_value"         -> am.zero_value
  | "extra_intf"         -> concat am.extra_intf
  | "extra_impl"         -> concat am.extra_impl
  | _                    -> failwith ("unknown variable '" ^ name ^ "'")

let array_modules = [
  array_module
    ~module_name:"JavaBooleanArray"
    ~java_element_type:"boolean"
    ~zero:"[false]"
    ~zero_value:"false"
    () ;
  array_module
    ~module_name:"JavaByteArray"
    ~java_element_type:"byte"
    ~zero_value:"0"
    ~extra_intf:[
      "" ;
      "(** {6 String operations} *)" ;
      "" ;
      "external to_ocaml_string : e java_byte_array -> string =" ;
      "  \"string_of_byte_array\"" ;
      "(** [to_string x] returns the array [x] as wrapped into a string. *)" ;
      "" ;
      "external of_ocaml_string : string -> e java_byte_array =" ;
      "  \"byte_array_of_string\"" ;
      "(** [of_string x] returns the array wrapped inside [x]. *)" ;
    ]
    ~extra_impl:[
      "external to_ocaml_string : e java_byte_array -> string =" ;
      "  \"string_of_byte_array\"" ;
      "" ;
      "external of_ocaml_string : string -> e java_byte_array =" ;
      "  \"byte_array_of_string\"" ;
    ]
    () ;
  array_module
    ~module_name:"JavaCharArray"
    ~java_element_type:"char"
    ~zero_value:"0"
    () ;
  array_module
    ~module_name:"JavaDoubleArray"
    ~java_element_type:"double"
    ~zero_value:"0."
    () ;
  array_module
    ~module_name:"JavaFloatArray"
    ~java_element_type:"float"
    ~zero_value:"0."
    () ;
  array_module
    ~module_name:"JavaIntArray"
    ~java_element_type:"int"
    ~zero_value:"0l"
    () ;
  array_module
    ~module_name:"JavaLongArray"
    ~java_element_type:"long"
    ~zero_value:"0L"
    () ;
  array_module
    ~module_name:"JavaShortArray"
    ~java_element_type:"short"
    ~zero_value:"0"
    ()
]

type number_module = {
    number_module_name    : string;
    java_primitive        : string;
    java_wrapper          : string;
    ocaml_type            : string;
    name                  : string;
    extra_constants_intf  : string list;
    extra_constants_impl  : string list;
    extra_operations_intf : string list;
    extra_operations_impl : string list;
  }

let number_module
    ~module_name
    ?(is_fp = false)
    ~java_primitive
    ?(java_wrapper = "")
    ?(ocaml_type = "")
    ?(name = "")
    ?(extra_constants_intf = [])
    ?(extra_constants_impl = [])
    ?(extra_operations_intf = [])
    ?(extra_operations_impl = [])
    () =
  let java_wrapper =
    if java_wrapper = "" then
      String.capitalize java_primitive
    else
      java_wrapper in
  let ocaml_type =
    if ocaml_type = "" then
      "java_" ^ java_primitive
    else
      ocaml_type in
  let name =
    if name = "" then
      java_primitive
    else
      name in
  let extra_constants_intf,
      extra_constants_impl,
      extra_operations_intf,
      extra_operations_impl =
    let java_primitive_capitalized = String.capitalize java_primitive in
    if is_fp then
      [ "val nan : " ^ ocaml_type ;
        "(** The not-a-number constant. *)" ;
        "" ;
        "val negative_infinity : " ^ ocaml_type ;
        "(** The constant for the negative infinity. *)" ;
        "" ;
        "val positive_infinity : " ^ ocaml_type ;
        "(** The constant for the positive infinity. *)" ]
      @ extra_constants_intf,
      [ "let nan = Java.get \"" ^ java_wrapper ^ ".NaN\" ()" ;
        "" ;
        "let negative_infinity = Java.get \"" ^ java_wrapper ^ ".NEGATIVE_INFINITY\" ()" ;
        "" ;
        "let positive_infinity = Java.get \"" ^ java_wrapper ^ ".POSITIVE_INFINITY\" ()" ]
      @ extra_constants_impl,
      [ "val parse_" ^ java_primitive ^ " : JavaString.t -> " ^ ocaml_type ;
        "(** Converts the passed string into a " ^ name ^ " wrapper; see" ;
        "    {java java.lang." ^ java_wrapper ^ "#parse" ^ java_primitive_capitalized ^ "(java.lang.String)}." ;
        "" ;
        "    @raise Java_exception if the string is invalid *)" ;
        "" ;
        "val value_of_string : JavaString.t -> t" ;
        "(** Converts the passed string into a " ^ name ^ " wrapper; see" ;
        "    {java java.lang." ^ java_wrapper ^ "#valueOf(java.lang.String)}." ;
        "" ;
        "    @raise Java_exception if the string is invalid *)" ]
      @ extra_operations_intf,
      [ "let parse_" ^ java_primitive ^ " str=" ;
        "  Java.call \"" ^ java_wrapper ^ ".parse" ^ java_primitive_capitalized ^ "(String)\" str" ;
        "" ;
        "let value_of_string str =" ;
        "  Java.call \"" ^ java_wrapper ^ ".valueOf(String)\" str" ]
      @ extra_operations_impl
    else
      extra_constants_intf,
      extra_constants_impl,
      [ "val parse_" ^ java_primitive ^ " : ?radix:java_int -> JavaString.t -> " ^ ocaml_type ;
        "(** Converts the passed string into a " ^ name ^ " wrapper, using the passed" ;
        "    radix (defaulting to [10l]); see" ;
        "    {java java.lang." ^ java_wrapper ^ "#parse" ^ java_primitive_capitalized ^ "(java.lang.String, int)}." ;
        "" ;
        "    @raise Java_exception if the string is invalid *)" ;
        "" ;
        "val decode : JavaString.t -> t" ;
        "(** Converts the passed string into a " ^ name ^ " wrapper; see" ;
        "    {java java.lang." ^ java_wrapper ^ "#decode(java.lang.String)}." ;
        "" ;
        "    @raise Java_exception if the string is invalid *)" ;
        "" ;
        "val value_of_string : ?radix:java_int -> JavaString.t -> t" ;
        "(** Converts the passed string into a " ^ name ^ " wrapper, using the passed" ;
        "    radix (defaulting to [10l]); see" ;
        "    {java java.lang." ^ java_wrapper ^ "#valueOf(java.lang.String, int)}." ;
        "" ;
        "    @raise Java_exception if the string is invalid *)" ]
      @ extra_operations_intf,
      [ "let parse_" ^ java_primitive ^ " ?(radix = 10l) str=" ;
        "  Java.call \"" ^ java_wrapper ^ ".parse" ^ java_primitive_capitalized ^ "(String,int)\" str radix" ;
        "" ;
        "let decode str =" ;
        "  Java.call \"" ^ java_wrapper ^ ".decode(String)\" str" ;
        "" ;
        "let value_of_string ?(radix = 10l) str =" ;
        "  Java.call \"" ^ java_wrapper ^ ".valueOf(String,int)\" str radix" ]
      @ extra_operations_impl in
  { number_module_name = module_name;
    java_primitive;
    java_wrapper;
    ocaml_type;
    name;
    extra_constants_intf;
    extra_constants_impl;
    extra_operations_intf;
    extra_operations_impl }

let subst_of_number_module nm name =
  let concat l =
    if l <> [] then
      "\n" ^ (String.concat "\n" l) ^ "\n"
    else
      "" in
  match name with
  | "module_name"                -> nm.number_module_name
  | "java_primitive"             -> nm.java_primitive
  | "java_primitive_capitalized" -> String.capitalize nm.java_primitive
  | "java_wrapper"               -> nm.java_wrapper
  | "ocaml_type"                 -> nm.ocaml_type
  | "name"                       -> nm.name
  | "extra_constants_intf"       -> concat nm.extra_constants_intf
  | "extra_constants_impl"       -> concat nm.extra_constants_impl
  | "extra_operations_intf"      -> concat nm.extra_operations_intf
  | "extra_operations_impl"      -> concat nm.extra_operations_impl
  | _                            -> failwith ("unknown variable '" ^ name ^ "'")

let number_modules = [
  number_module
    ~module_name:"JavaByte"
    ~java_primitive:"byte"
    () ;
  number_module
    ~module_name:"JavaDouble"
    ~is_fp:true
    ~java_primitive:"double"
    () ;
  number_module
    ~module_name:"JavaFloat"
    ~is_fp:true
    ~java_primitive:"float"
    () ;
  number_module
    ~module_name:"JavaInt"
    ~java_primitive:"int"
    ~java_wrapper:"Integer"
    ~name:"integer"
    () ;
  number_module
    ~module_name:"JavaLong"
    ~java_primitive:"long"
    () ;
  number_module
    ~module_name:"JavaShort"
    ~java_primitive:"short"
    ()
]

type exception_module = {
    exception_module_name : string;
    java_full_name        : string;
    java_short_name       : string;
    ocaml_full_name       : string;
    ocaml_short_name      : string;
    module_ocamldoc       : string;
    type_ocamldoc         : string;
  }

let exception_module
    ~module_name
    ~java_full_name
    ~java_short_name
    ~ocaml_full_name
    ~ocaml_short_name
    ~module_ocamldoc
    ~type_ocamldoc
    () =
  { exception_module_name = module_name;
    java_full_name;
    java_short_name;
    ocaml_full_name;
    ocaml_short_name;
    module_ocamldoc;
    type_ocamldoc; }

let subst_of_exception_module em name =
  match name with
  | "module_name"      -> em.exception_module_name
  | "java_full_name"   -> em.java_full_name
  | "java_short_name"  -> em.java_short_name
  | "ocaml_full_name"  -> em.ocaml_full_name
  | "ocaml_short_name" -> em.ocaml_short_name
  | "module_ocamldoc"  -> em.module_ocamldoc
  | "type_ocamldoc"    -> em.type_ocamldoc
  | _                  -> failwith ("unknown variable '" ^ name ^ "'")

let exception_modules = [
  exception_module
    ~module_name:"JavaThrowable"
    ~java_full_name:"java.lang.Throwable"
    ~java_short_name:"Throwable"
    ~ocaml_full_name:"java'lang'Throwable"
    ~ocaml_short_name:"_'Throwable"
    ~module_ocamldoc:"Utility functions for Java throwables."
    ~type_ocamldoc:"The type of throwables."
    () ;
  exception_module
    ~module_name:"JavaError"
    ~java_full_name:"java.lang.Error"
    ~java_short_name:"Error"
    ~ocaml_full_name:"java'lang'Error"
    ~ocaml_short_name:"_'Error"
    ~module_ocamldoc:"Utility functions for Java errors."
    ~type_ocamldoc:"The type of errors."
    () ;
  exception_module
    ~module_name:"JavaException"
    ~java_full_name:"java.lang.Exception"
    ~java_short_name:"Exception"
    ~ocaml_full_name:"java'lang'Exception"
    ~ocaml_short_name:"_'Exception"
    ~module_ocamldoc:"Utility functions for Java exceptions."
    ~type_ocamldoc:"The type of exceptions."
    ()
]

let () =
  let odocl_chan = open_out odocl_file in
  let mllib_chan = open_out mllib_file in
  let add_module module_name =
    output_string odocl_chan module_name;
    output_char odocl_chan '\n';
    output_string mllib_chan module_name;
    output_char mllib_chan '\n' in
  let generated_modules =
      (List.map (fun am -> am.array_module_name)     array_modules)
    @ (List.map (fun nm -> nm.number_module_name)    number_modules)
    @ (List.map (fun em -> em.exception_module_name) exception_modules) in
  List.iter add_module generated_modules;
  let add_file filename =
    if (Pathname.check_extension filename "mli")
      || (Pathname.check_extension filename "mly")
      || (Pathname.check_extension filename "mll") then begin
          let module_name =
            filename
            |> Pathname.remove_extension 
            |> Pathname.basename
            |> String.capitalize in
          if not (List.mem module_name excluded_modules) then
            add_module module_name
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
  let safe_mv src dst =
    let src = Pathname.mk src in
    let dst = Pathname.mk dst in
    let dir = Pathname.dirname dst in
    let cmd = Printf.sprintf "mkdir -p %s" (Pathname.to_string dir) in
    if Sys.command cmd <> 0 then failwith ("cannot run " ^ cmd);
    mv src dst in
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
  let make_rules get_name get_subst template_base modules =
    List.iter
      (fun modul ->
        let make_rule suffix =
          let file_path =
            "src/" ^ (String.uncapitalize (get_name modul)) ^ suffix in
          rule file_path
            ~prod:file_path
            ~insert:`bottom
            (fun _ _ ->
              let name, chan = Filename.open_temp_file "template" suffix in
              let template = open_in (template_base ^ suffix) in
              apply_template template chan (get_subst modul);
              close_out_noerr chan;
              safe_mv name file_path) in
        make_rule ".ml";
        make_rule ".mli")
      modules in
  dispatch begin function
    | After_rules ->
        flag ["ocaml"; "compile"; "warnings"] (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        dep  ["needs-java-pervasives"]        ["src/javaPervasives.cmi"];
        make_rules
          (fun am -> am.array_module_name)
          subst_of_array_module
          "../templates/array-template"
          array_modules;
        make_rules
          (fun nm -> nm.number_module_name)
          subst_of_number_module
          "../templates/number-template"
          number_modules;
        make_rules
          (fun em -> em.exception_module_name)
          subst_of_exception_module
          "../templates/throwable-template"
          exception_modules;
    | _ -> ()
  end
