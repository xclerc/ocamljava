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


open Wrap_common

let wrap name fields record_repr type_parameters =
  let open JavaAST in
  let open Types in
  TypeMap.add_local name false;
  let generics = TypeParametersTable.make type_parameters in
  let infos =
    List.mapi
      (fun i (_, _, type_expr) ->
        i, TypeMap.find ~generics false type_expr)
      fields in
  let wrapper_fields, get_wrapper_meth, get_wrapper_idx_meth, wrapper_meth, wrap_meth =
    Wrap_common.make_wrapper_elements (Ident.name name) type_parameters in
  let cstr =
    let wrapper_parameters, inits =
      Wrap_common.make_wrapper_cstr_elements wrapper_fields type_parameters in
    constructor
      [Private] (Ident.name name) ~parameters:(wrapper_parameters @ [type_Value, "v"])
      ([Super_constructor [Identifier "v"]] @ inits) in
  let accessors =
    List.map2
      (fun (id, mut, _) (pos, info) ->
        let get =
          let res =
            if record_repr = Record_regular then
              let tmp = get (Identifier "this.value") pos in
              info.TypeInfo.java_of_ocaml tmp
            else
              get_double (Identifier "this.value") pos in
          method_
            [Public] ~return:(Some (info.TypeInfo.java_type))
            ("get" ^ (String.capitalize (Ident.name id)))
            [Return (Some (cast_if_needed info.TypeInfo.java_type res))] in
        if mut = Asttypes.Immutable then
          [get]
        else begin
          let x = Identifier "x" in
          let call =
            if record_repr = Record_regular then
              let x = info.TypeInfo.ocaml_of_java x in
              set (Identifier "this.value") pos x
            else
              set_double (Identifier "this.value") pos x in
          let set =
            method_
              [Public] ("set" ^ (String.capitalize (Ident.name id)))
              ~parameters:[info.TypeInfo.java_type, "x"]
              [Expression (call)] in
          [get; set]
        end)
      fields
      infos in
  let create =
    let quantif =
      List.map
        (fun (id, _) -> Printf.sprintf "%s extends OCamlValue" id)
        type_parameters in
    let wrappers =
      List.map
        (fun (_, idx) ->
          let id = Identifier (Printf.sprintf "v%d" idx) in
          Call (id, "getWrapper", []))
        type_parameters in
    method_
      ~generics:quantif
      ~annotations:(if type_parameters = [] then [] else ["@SuppressWarnings(\"unchecked\")"])
      [Public; Static]
      ~return:(Some (Reference (Ident.name name, [])))
      "create"
      ~parameters:(List.map2
                     (fun (_, _, _) (pos, info) -> (info.TypeInfo.java_type, Printf.sprintf "v%d" pos))
                     fields
                     infos)
      (let l =
        List.map2
          (fun (_, _, _) (pos, _info) ->
            Identifier (Printf.sprintf "v%d" pos))
          fields
          infos in
      let l' =
        List.map2
          (fun e (_, info) ->
              let e : JavaAST.expression = e in
              let info : TypeInfo.t = info in
              let conv : TypeInfo.conversion_function = info.TypeInfo.ocaml_of_java in
              if record_repr = Record_regular then
                conv e
              else
                e)
          l
          infos in
      let wrap =
        if record_repr = Record_regular then
          create_block 0 l'
        else
          create_double_array l' in
        let res = New (Ident.name name, (wrappers @ [wrap])) in
        [ Return (Some res) ]) in
  let accessors = List.flatten accessors in
  let hash_code =
    method_
      ~annotations:["@Override"]
      [Public]
      ~return:(Some Int)
      "hashCode"
      (let call = Call (Identifier "this.value", "hashCode", []) in
      [Return (Some call)]) in
  let equals =
    method_
      ~annotations:["@Override"]
      [Public]
      ~return:(Some Boolean)
      "equals"
      ~parameters:[type_Object, "obj"]
      (let cond = Instance_of (Identifier "obj", Reference (Ident.name name, [])) in
      let if_branch = 
        let t = Reference (Ident.name name, []) in
        let l =
          List.map
            (fun (pos, info) ->
              let this =
                if record_repr = Record_regular then
                  let tmp = get (Identifier "this.value") pos in
                  info.TypeInfo.java_of_ocaml tmp
                else
                  get_double (Identifier "this.value") pos in
              let that =
                 if record_repr = Record_regular then
                   let tmp = get (Identifier "that.value") pos in
                   info.TypeInfo.java_of_ocaml tmp
                 else
                   get_double (Identifier "that.value") pos in
              if primitive_type info.TypeInfo.java_type then
                Infix ("==", this, that)
              else
                Call (this, "equals", [that]))
            infos in
        Block [
        Variable_declaration (t, "that", Cast (t, Identifier "obj"));
        Return (Some (JavaAST.and_list l))
      ] in
      let else_branch = Return (Some (Identifier "false")) in
      [If_else (cond, if_branch, else_branch)]) in
  let to_string =
    method_
      ~annotations:["@Override"]
      [Public]
      ~return:(Some type_String)
      "toString"
      (let string_builder = Reference ("StringBuilder", []) in
      let append e = Expression (Call (Identifier "sb", "append", [e])) in
      [ Variable_declaration (string_builder, "sb", New ("StringBuilder", []));
        append (String_literal ((Ident.name name) ^ "("))]
      @ (List.flatten (List.map2
                         (fun (id, _, _) (pos, info) ->
                           let elem =
                             if record_repr = Record_regular then
                               let tmp = get (Identifier "this.value") pos in
                               info.TypeInfo.java_of_ocaml tmp
                             else
                               get_double (Identifier "this.value") pos in
                           let pref = Printf.sprintf "%s%s="
                               (if pos > 0 then ", " else "")
                               (Ident.name id) in
                           [append (String_literal pref) ;
                            append elem])
                         fields
                         infos))
      @ [ append (String_literal ")");
          Return (Some (Call (Identifier "sb", "toString", []))) ]) in
  let name = Ident.name name in
  let full_name =
    if type_parameters = [] then
      name
    else
      let tmp =
        List.map
          (fun (id, _) ->
            Printf.sprintf "%s extends OCamlValue" id)
          type_parameters in
      name ^ "<" ^ (String.concat ", " tmp) ^ ">" in
  class_
    [Public; Static; Final] full_name ~extends:(Some "OCamlValue")
    ~fields:wrapper_fields
    ~methods:([cstr] @ [get_wrapper_meth; get_wrapper_idx_meth] @ accessors @
              [hash_code; equals; to_string] @
              [create] @ [wrap_meth; wrapper_meth])
    ()
