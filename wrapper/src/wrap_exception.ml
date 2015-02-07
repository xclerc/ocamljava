(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2015 Xavier Clerc.
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


open Wrap_common

let wrap ident exn_args global_idx =
  let open JavaAST in
  let name = Ident.name ident in
  Output.verbose (Printf.sprintf "  wrapping exception %S..." name);
  let exn_args = (* name, info *)
    List.mapi
      (fun i arg ->
        let info = TypeMap.find false arg in
        let name = Printf.sprintf "p%d" i in
        info, name)
      exn_args in
  let formal_params =
    List.map
      (fun (info, name) ->
        let typ = info.TypeInfo.java_type in
        typ, name)
      exn_args in
  let identifiers =
    List.map
      (fun (_, x) -> Identifier x)
      formal_params in
  let cstr =
    constructor
      [Public] name ~parameters:formal_params
      [Super_constructor [Static_call (name, "create", identifiers)]] in
  let global_tag = get_global global_idx in
  let create_meth =
    let elems =
      List.map
        (fun (info, name) ->
          info.TypeInfo.ocaml_of_java (Identifier name))
        exn_args in
    let elems = global_tag :: elems in
    method_
      [Private; Static] ~return:(Some type_Value) "create" ~parameters:formal_params
      [return (create_block 0 elems)] in
  let global_id = Call (get global_tag 0, "asString", []) in
  let register =
    Static_call ("OCamlException", "register", [global_id; Identifier (name ^ ".class")]) in
  add_static_block [ Expression register ];
  class_
    [Public; Static; Final] name ~extends:(Some "OCamlException")
    ~methods:[cstr; create_meth] ()
