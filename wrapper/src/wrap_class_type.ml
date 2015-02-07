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

let extract_methods self =
  let open Types in
  let rec extract acc te =
    match te.desc with
    | Tfield ("*dummy method*", _, _, te') ->
        extract acc te'
    | Tfield (name, Fpresent, te1, te2) ->
        let acc =
          match te1.desc with
          | Tpoly (te, _) -> (name, te, None) :: acc
          | _ -> acc in
        extract acc te2
    | Tfield (_, _, _, te') ->
        extract acc te'
    | _ -> List.rev acc in
  match self.desc with
  | Tobject (te, _) -> extract [] te
  | _ -> []

let wrap name class_type_declaration =
  let class_name = Ident.name name in
  Output.verbose (Printf.sprintf "  wrapping class type %S..." class_name);
  TypeMap.add_local name true;
  let open Types in
  let open JavaAST in
  let type_parameters =
    List.mapi
      (fun i te ->
        match te.desc with
        | Tvar (Some id) -> id, i
        | _ -> assert false)
      class_type_declaration.clty_params in
  let wrapper_fields, get_wrapper_meth, get_wrapper_idx_meth, wrapper_meth, wrap_meth =
    Wrap_common.make_wrapper_elements (Ident.name name) ~suffix:"$impl" type_parameters in
  let cstr, cstrc, cstr2 =
    let wrapper_parameters, inits =
      Wrap_common.make_wrapper_cstr_elements wrapper_fields type_parameters in
    let c1 =
      constructor
        [Public] (Ident.name name) ~parameters:wrapper_parameters
        ([Super_constructor [Identifier "Value.UNIT"];
          Assign ("this.oid", Static_call ("OCamlWrappers", "getAndIncrementOid", []))] @ inits) in
    let cc =
      constructor
        [Protected] (Ident.name name) ~parameters:(wrapper_parameters @ [type_Value, "v"])
        ([Super_constructor [Identifier "v"];
          Assign ("this.oid", Identifier "Value.MINUS_ONE")] @ inits) in
    let effective_parameters = List.map (fun (_, id) -> Identifier id) wrapper_parameters in
    let c2 =
      constructor
        ~annotations:(if effective_parameters = [] then [] else ["@SuppressWarnings(\"unchecked\")"])
        [Public] ((Ident.name name) ^ "$impl") ~parameters:(wrapper_parameters @ [type_Value, "v"])
        [Super_constructor (effective_parameters @ [Identifier "v"])] in
    c1, cc, c2 in
  let methods, values =
    match class_type_declaration.clty_type with
    | Cty_signature x ->
        if x.cty_inher <> [] then fail Cannot_inherit;
        extract_methods x.cty_self, x.cty_vars
    | _ -> fail (Cannot_translate_class_type name) in
  if not (Types.Vars.is_empty values) then fail Cannot_contain_value;
  let implementations, callers, closures =
    Wrap_structure.make_methods true methods in
  let create_long x =
    Static_call ("Value", "createLong", [Int_literal (Int32.of_int x)]) in
  let create_method_table_meth =
    let sz = 2 + (2 * (List.length methods)) in
    let sz = Int32.of_int sz in
    let res = Identifier "res" in
    let l =
      List.map
        (fun m ->
          let hash = Btype.hash_variant m.meth_name in
          let arity = List.length m.meth_parameters in
          hash, m.meth_name, succ arity)
        implementations in
    let l = List.sort Pervasives.compare l in
    let l =
      List.mapi
        (fun i (hash, name, arity) ->
          let clos =
            Static_call ("OCamlWrappers",
                         "createClosure",
                         [ Identifier (class_name ^ ".class");
                           String_literal (name ^ "$impl");
                           Identifier "this";
                           Int_literal (Int32.of_int arity) ]) in
          let idx = 2 + (i * 2) in
          [Expression (set res idx clos);
           Expression (set res (succ idx) (create_long hash))])
        l in
    let l = List.flatten l in
    method_
      [Private]
      ~return:(Some type_Value)
      "createMethodTable"
      ~parameters:[]
      ([Variable_declaration (type_Value, "res",
                              Static_call ("Value",
                                           "createBlock",
                                           [Int_literal 0l; Int_literal sz]));
        Expression (set res 0 (create_long (List.length methods)));
        Expression (set res 1 (create_long 0))]
       @ l
       @ [return res]) in
  let value_meth =
    let cached_id = Identifier "this.cached" in
    let sz = Int32.of_int (2 + 0 (*List.length values*)) in
    let build_cached =
      [Assign ("this.cached",
               Static_call ("Value",
                            "createBlock",
                            [Identifier "BlockValue.OBJECT_TAG"; Int_literal sz]));
       Expression (set cached_id 0 (Call (Identifier "this", "createMethodTable", [])));
       Expression (set cached_id 1 (Identifier "this.oid"))]
      (*@ (List.mapi
           (fun i (m, conv) ->
             let v = Call (Identifier "this", m.meth_name, []) in
             let s = set cached_id (i + 2) (conv v) in
             Expression s)
           values*) in
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some type_Value) "value"
      [If (Infix ("==", cached_id, Null_literal), Block build_cached);
       JavaAST.return cached_id] in
  let name = Ident.name name in
  let full_name name =
    if type_parameters = [] then
      name
    else
      let tmp =
        List.map
          (fun (id, _) ->
            Printf.sprintf "%s extends OCamlValue" id)
          type_parameters in
      name ^ "<" ^ (String.concat ", " tmp) ^ ">" in
  let full_name' name =
    if type_parameters = [] then
      name
    else
      let tmp =
        List.map
          (fun (id, _) ->
            Printf.sprintf "%s" id)
          type_parameters in
      name ^ "<" ^ (String.concat ", " tmp) ^ ">" in
  let oid_field = [Private; Final], type_Value, "oid", None in
  let cached_field = [Private], type_Value, "cached", None in
  let hash_code, equals, to_string =
    make_basic_object_methods name "oid" false in
  let abstract =
    class_
      [Public; Static; Abstract] (full_name name) ~extends:(Some "OCamlValue")
      ~fields:([oid_field; cached_field] @ wrapper_fields)
      ~methods:([cstr; cstrc; value_meth; create_method_table_meth]
                @ implementations
                @ closures
                @ [hash_code; equals; to_string]
                @ [get_wrapper_meth;
                   get_wrapper_idx_meth;
                   wrap_meth;
                   wrapper_meth])
      () in
  let orig_value_meth =
    method_
      ~annotations:["@Override"]
      [Public; Final] ~return:(Some type_Value) "value"
      [return (Identifier "this.value")] in
  let final =
    class_
      [Private; Static; Final] (full_name (name ^ "$impl")) ~extends:(Some (full_name' name))
      ~methods:([cstr2] @ callers @ [orig_value_meth])
      ()
 in
  abstract, final
