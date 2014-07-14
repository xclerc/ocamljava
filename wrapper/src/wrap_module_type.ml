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

let elements_of_signature signature =
  let open Types in
  let types, functions, _ =
    List.fold_left
      (fun (acc_types, acc_functions, idx) elem ->
        match elem with
        | Sig_type (id,
                    { type_params = [];
                      type_arity = 0;
                      type_kind = Type_abstract;
                      type_manifest = None;
                      _ },
                    _) ->
                      (Ident.name id, List.length acc_types)
                      :: acc_types, acc_functions, idx
        | Sig_value (id,
                     { val_type = { desc = (Tarrow _); _ } as type_expr;
                       val_kind = Val_reg;
                       _ }) ->
                         acc_types, (Ident.name id, type_expr, Some idx)
                         :: acc_functions, succ idx
        | Sig_value _
        | Sig_type _      
        | Sig_exception _
        | Sig_module _
        | Sig_modtype _
        | Sig_class _
        | Sig_class_type _ -> fail Only_asbtract_types_and_functions)
      ([], [], 0)
      signature in
  List.rev types, List.rev functions

let rec patch_function types type_expr =
  let open Types in
  let desc =
    match type_expr.desc with
    | Tarrow (x, te1, te2, c) ->
        let te1 = patch_function types te1 in
        let te2 = patch_function types te2 in
        Tarrow (x, te1, te2, c)
    | Ttuple l -> Ttuple (patch_function_list types l)
    | Tconstr (path, _, _) when List.mem_assoc (Path.name path) types ->
        Tvar (Some (Path.name path))
    | Tconstr (path, l, a) ->
        Tconstr (path, patch_function_list types l, a)
    | Tlink te -> Tlink (patch_function types te)
    | x -> x in
  { type_expr with desc }
and patch_function_list types type_expr_list =
  List.map (patch_function types) type_expr_list

let wrap name signature =
  let class_name = Ident.name name in
  Output.verbose (Printf.sprintf "  wrapping module type %S..." class_name);
  TypeMap.add_local name true;
  let open JavaAST in
  let type_parameters, functions = elements_of_signature signature in
  let wrapper_fields, get_wrapper_meth, get_wrapper_idx_meth, wrapper_meth, wrap_meth =
    Wrap_common.make_wrapper_elements (Ident.name name) ~suffix:"$impl" type_parameters in
  let cstr, cstrc, cstr2 =
    let wrapper_parameters, inits =
      Wrap_common.make_wrapper_cstr_elements wrapper_fields type_parameters in
    let c1 =
      constructor
        [Public] (Ident.name name) ~parameters:wrapper_parameters
        ([Super_constructor [Identifier "Value.UNIT"]] @ inits) in
    let cc =
      constructor
        [Protected] (Ident.name name) ~parameters:(wrapper_parameters @ [type_Value, "v"])
        ([Super_constructor [Identifier "v"]] @ inits) in
    let effective_parameters = List.map (fun (_, id) -> Identifier id) wrapper_parameters in
    let c2 =
      constructor
        ~annotations:(if effective_parameters = [] then [] else ["@SuppressWarnings(\"unchecked\")"])
        [Public] ((Ident.name name) ^ "$impl") ~parameters:(wrapper_parameters @ [type_Value, "v"])
        [Super_constructor (effective_parameters @ [Identifier "v"])] in
    c1, cc, c2 in
  let functions =
    List.map
      (fun (id, type_expr, idx) ->
        id, patch_function type_parameters type_expr, idx)
      functions in
  let implementations, callers, closures =
    Wrap_structure.make_methods false functions in
  let value_meth =
    let cached_id = Identifier "this.cached" in
    let sz = Int32.of_int (List.length implementations) in
    let build_cached =
      [Assign ("this.cached",
               Static_call ("Value",
                            "createBlock",
                            [Int_literal 0l; Int_literal sz]))]
      @ (List.mapi
           (fun i m ->
             let arity = List.length m.meth_parameters in
             let close =
               Static_call ("OCamlWrappers",
                            "createClosure",
                            [ Identifier (class_name ^ ".class");
                              String_literal m.meth_name;
                              Identifier "this";
                              Int_literal (Int32.of_int arity) ]) in
             let s = set cached_id i close in
             Expression s)
           closures) in
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
  let cached_field = [Private], type_Value, "cached", None in
  let hash_code, equals, to_string =
    make_basic_object_methods name "" false in
  let abstract =
    class_
      [Public; Static; Abstract] (full_name name) ~extends:(Some "OCamlValue")
      ~fields:([cached_field] @ wrapper_fields)
      ~methods:([cstr; cstrc; value_meth]
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
  abstract, final, List.map fst type_parameters
