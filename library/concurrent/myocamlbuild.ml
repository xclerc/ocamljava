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

let odocl_file = Pathname.pwd / "concurrent.odocl"
let mllib_file = Pathname.pwd / "concurrent.mllib"
let src_path   = Pathname.pwd / "src"
let excluded_modules = []

type atomic_module = {
    atomic_module_name : string;
    ocaml_type         : string;
    java_class         : string;
    java_type          : string;
    ocaml_java_type    : string;
    is_integer         : bool;
    is_array           : bool;
  }

let atomic_module
    ~module_name
    ~ocaml_type
    ~java_class
    ~java_type
    ~ocaml_java_type
    ~is_integer
    ~is_array =
  { atomic_module_name = module_name;
    ocaml_type;
    java_class;
    java_type;
    ocaml_java_type;
    is_integer;
    is_array; }

let subst_of_atomic_module am signature name =
  match name with
  | "module_name"     -> am.atomic_module_name
  | "ocaml_type"      -> am.ocaml_type
  | "java_class"      -> am.java_class
  | "java_type"       -> am.java_type
  | "ocaml_java_type" -> am.ocaml_java_type
(* functions for integer types *)
  | "add_and_get" | "get_and_add" ->
      if am.is_integer then begin
        if signature then
          let ocamldoc =
            if name = "add_and_get" then
              "atomically adds [d] to the current value, and returns the new value"
            else
              "atomically adds [d] to the current value, and returns the previous value" in
          Printf.sprintf
            "val %s : t -> %s -> %s\n\
            (** [%s a d] %s. *)\n"
            name am.ocaml_type am.ocaml_type name ocamldoc
        else
          let java_name =
            if name = "add_and_get" then
              "addAndGet"
            else
              "getAndAdd" in
          Printf.sprintf
            "let %s atomic delta = Java.call \"%s.%s(%s)\" atomic delta\n"
            name am.java_class java_name am.java_type
      end else
        ""
  | "decrement_and_get" | "increment_and_get"
  | "get_and_decrement" | "get_and_increment" ->
      let java_name, increment, get_first =
        match name with
        | "decrement_and_get" -> "decrementAndGet", false, false
        | "increment_and_get" -> "incrementAndGet", true,  false
        | "get_and_decrement" -> "getAndDecrement", false, true
        | "get_and_increment" -> "getAndIncrement", true,  true
        | _                   -> assert false in
      if am.is_integer then begin
        if signature then
          let ocamldoc =
            Printf.sprintf "Atomically %s the current value, and returns the %s value"
              (if increment then "increments" else "decrements")
              (if get_first then "previous"   else "new") in
          Printf.sprintf
            "val %s : t -> %s\n\
            (** %s. *)\n"
            name am.ocaml_type ocamldoc
        else
          Printf.sprintf
            "let %s atomic = Java.call \"%s.%s()\" atomic\n"
            name am.java_class java_name
      end else
        ""
  | _ -> failwith ("unknown variable '" ^ name ^ "'")

let atomic_modules = [
  atomic_module
    ~module_name:"AtomicBool"
    ~ocaml_type:"bool"
    ~java_class:"java.util.concurrent.atomic.AtomicBoolean"
    ~java_type:"boolean"
    ~ocaml_java_type:"java'util'concurrent'atomic'AtomicBoolean java_instance"
    ~is_integer:false
    ~is_array:false;
  atomic_module
    ~module_name:"AtomicInt32"
    ~ocaml_type:"int32"
    ~java_class:"java.util.concurrent.atomic.AtomicInteger"
    ~java_type:"int"
    ~ocaml_java_type:"java'util'concurrent'atomic'AtomicInteger java_instance"
    ~is_integer:true
    ~is_array:false;
  atomic_module
    ~module_name:"AtomicInt64"
    ~ocaml_type:"int64"
    ~java_class:"java.util.concurrent.atomic.AtomicLong"
    ~java_type:"long"
    ~ocaml_java_type:"java'util'concurrent'atomic'AtomicLong java_instance"
    ~is_integer:true
    ~is_array:false;
  atomic_module
    ~module_name:"AtomicInt32Array"
    ~ocaml_type:"int32"
    ~java_class:"java.util.concurrent.atomic.AtomicIntegerArray"
    ~java_type:"int"
    ~ocaml_java_type:"java'util'concurrent'atomic'AtomicIntegerArray java_instance"
    ~is_integer:false
    ~is_array:true;
  atomic_module
    ~module_name:"AtomicInt64Array"
    ~ocaml_type:"int64"
    ~java_class:"java.util.concurrent.atomic.AtomicLongArray"
    ~java_type:"long"
    ~ocaml_java_type:"java'util'concurrent'atomic'AtomicLongArray java_instance"
    ~is_integer:false
    ~is_array:true;
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
    List.map (fun am -> am.atomic_module_name) atomic_modules in
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
  let make_rules get_name get_subst get_template_base modules =
    List.iter
      (fun modul ->
        let make_rule signature suffix =
          let file_path =
            "src/atomic/" ^ (String.uncapitalize (get_name modul)) ^ suffix in
          rule file_path
            ~prod:file_path
            ~insert:`bottom
            (fun _ _ ->
              let name, chan = Filename.open_temp_file "template" suffix in
              let template = open_in ((get_template_base modul) ^ suffix) in
              apply_template template chan ((get_subst modul) signature);
              close_out_noerr chan;
              safe_mv name file_path) in
        make_rule false ".ml";
        make_rule true  ".mli")
      modules in
  dispatch begin function
    | After_rules ->
        flag ["ocaml"; "compile"; "warnings"] (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        make_rules
          (fun am -> am.atomic_module_name)
          subst_of_atomic_module
          (fun am ->
            if am.is_array then
              "../templates/atomic-array-template"
            else
              "../templates/atomic-template")
          atomic_modules
    | _ -> ()
  end
