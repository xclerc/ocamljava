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


type error =
  | Command_line_inconsistency of string
  | Cannot_find_cmi_file of string
  | Invalid_cmi_file of string
  | Invalid_cmj_file of string
  | Cannot_determine_function of Ident.t
  | Cannot_translate_open_polymorphic_variant of Ident.t
  | Cannot_translate_polymorphic_variant of Ident.t
  | Cannot_translate_class_type of Ident.t
  | Cannot_map_type of string
  | Cannot_find_type of string
  | Tuple_is_too_large of int
  | Function_arity_is_too_large of int
  | Cannot_inherit
  | Cannot_contain_value
  | Only_asbtract_types_and_functions
  | Cannot_determine_functor_signature

exception Exception of error

let fail e = raise (Exception e)

let map_option l = 
  List.fold_right
    (fun elem acc ->
      match elem with
      | Some e -> e :: acc
      | None -> acc)
    l
    []

let string_of_error = function
  | Command_line_inconsistency s ->
      Printf.sprintf "command-line inconsistency: %s" s
  | Cannot_find_cmi_file s ->
      Printf.sprintf "cannot find cmi file %S" s
  | Invalid_cmi_file s ->
      Printf.sprintf "invalid cmi file %S" s
  | Invalid_cmj_file s ->
      Printf.sprintf "invalid cmj file %S" s
  | Cannot_determine_function i ->
      Printf.sprintf "cannot determine function for %S" (Ident.name i)
  | Cannot_translate_open_polymorphic_variant i ->
      Printf.sprintf "cannot translate open polymorphic variant %S" (Ident.name i)
  | Cannot_translate_polymorphic_variant i ->
      Printf.sprintf "cannot translate polymorphic variant %S" (Ident.name i)
  | Cannot_translate_class_type i ->
      Printf.sprintf "cannot translate class type %S" (Ident.name i)
  | Cannot_map_type s ->
      Printf.sprintf "cannot map type %S" s
  | Cannot_find_type s ->
      Printf.sprintf "cannot find type %S" s
  | Tuple_is_too_large n ->
      Printf.sprintf "tuple is too large (size is %d)" n
  | Function_arity_is_too_large n ->
      Printf.sprintf "function arity is too large (arity is %d)" n
  | Cannot_inherit ->
      "class type cannot inherit from a parent class type"
  | Cannot_contain_value ->
      "class type cannot contain value"
  | Only_asbtract_types_and_functions ->
      "module type can contain only abstract types and functions"
  | Cannot_determine_functor_signature ->
      "cannot determine functor signature"

let () =
  Printexc.register_printer
    (function | Exception e -> Some (string_of_error e) | _ -> None)

let main_static_block = ref []

let add_static_block l =
    main_static_block := !main_static_block @ l

let clear_static_block () =
  main_static_block := []

let get e idx =
  let open JavaAST in
  if (idx >= 0) && (idx <= 7) then
    Call (e, Printf.sprintf "get%d" idx, [])
  else
    Call (e, "get", [Int_literal (Int32.of_int idx)])

let set e idx e' =
  let open JavaAST in
  if (idx >= 0) && (idx <= 7) then
    Call (e, Printf.sprintf "set%d" idx, [e'])
  else
    Call (e, "set", [Int_literal (Int32.of_int idx); e'])

let get_double e idx =
  let open JavaAST in
  if (idx >= 0) && (idx <= 7) then
    Call (e, Printf.sprintf "getDouble%d" idx, [])
  else
    Call (e, "getDouble", [Int_literal (Int32.of_int idx)])

let set_double e idx e' =
  let open JavaAST in
  if (idx >= 0) && (idx <= 7) then
    Call (e, Printf.sprintf "setDouble%d" idx, [e'])
  else
    Call (e, "setDouble", [Int_literal (Int32.of_int idx); e'])

let get_global idx =
  let open JavaAST in
  match !State.java_class with
  | Some x ->
      let e = Static_call (x, "getGlobal", []) in
      get e idx
  | None ->
      assert false

let create_block tag expr_list =
  let open JavaAST in
  let tag = Int_literal (Int32.of_int tag) in
  if (List.length expr_list) <= 8 then
    Static_call ("Value", "createBlock", tag :: expr_list)
  else
    let array = New_array (type_Value, expr_list) in
    Static_call ("Value", "createBlock", [tag; array])

let create_double_array expr_list =
  let open JavaAST in
  if (List.length expr_list) <= 8 then
    Static_call ("Value", "createDoubleArray", expr_list)
  else
    let array = New_array (Double, expr_list) in
    Static_call ("Value", "createDoubleArray", [array])

let create_long x =
  let open JavaAST in
  Static_call ("Value", "createLong", [Int_literal (Int32.of_int x)])

let make_wrapper_elements_no_type_parameter class_name suffix =
  let open JavaAST in
  let wrapper_type =
    Reference ("Wrapper", [Reference ("? extends " ^ (checked_name class_name), [])]) in
  let wrapper_field =
    let ret_type = Reference ("Wrapper", [Reference (class_name, [])]) in
    [Public; Static; Final],
    ret_type,
    "WRAPPER",
    Some (Anonymous_class (Reference ("SimpleWrapper", [Reference (class_name, [])]),
                           [],
                           Reference (class_name, []),
                           "wrap",
                           [Reference ("Value", []), "v"],
                           New (class_name ^ suffix, [Identifier "v"]))) in
  let get_wrapper_meth =
    method_ [Public] ~return:(Some wrapper_type) "getWrapper"
      [Return (Some (Identifier ((checked_name class_name) ^ ".WRAPPER")))] in
  let get_wrapper_idx_meth =
    method_ [Public] ~return:(Some wrapper_type) "getWrapper" ~parameters:[Int, "idx"]
      [Return (Some (Identifier ((checked_name class_name) ^ ".WRAPPER")))] in
  let wrapper_meth =
    method_ [Public; Static] ~return:(Some wrapper_type) "wrapper"
      [Return (Some (Identifier ((checked_name class_name) ^ ".WRAPPER")))] in
  let class_type = Reference (class_name, []) in
  let wrap_meth =
    method_
      [Public; Static] ~return:(Some class_type) "wrap" ~parameters:[type_Value, "v"]
      [return (New (class_name ^ suffix, [Identifier "v"]))] in
  [wrapper_field], get_wrapper_idx_meth, get_wrapper_meth, wrapper_meth, wrap_meth

let make_wrapper_elements_type_parameter class_name suffix type_parameters =
  let open JavaAST in
  let sub_wrapper_types =
    List.map
      (fun (id, _) -> Reference (id, []))
      type_parameters in
  let wrapper_type =
    Reference ("Wrapper", [Reference ("? extends " ^ (checked_name class_name), sub_wrapper_types)]) in
  let wrapper_type'0 = Reference (class_name, sub_wrapper_types) in
  let wrapper_type' = Reference ("ComposedWrapper", [wrapper_type'0]) in
  let fields =
    List.map
      (fun (id, _) ->
        let type_ =
    Reference ("Wrapper", [Reference (id, [])]) in
        let name = "w" ^ id in
        [Public; Final],
        type_, name, None)
      type_parameters in
  let get_wrapper_meth =
    method_
      [Public] ~return:(Some wrapper_type) "getWrapper"
      [return (Static_call (class_name, "wrapper", List.map (fun (_, _, n, _) -> Identifier ("this."^n)) fields))] in
  let get_wrapper_idx_meth =
    method_
      [Public] ~return:(Some (Reference ("Wrapper", [Reference ("? extends OCamlValue", [])]))) "getWrapper" ~parameters:[Int, "idx"]
      [Switch (Identifier "idx",
               (List.mapi (fun i (_, _, n, _) -> Int32.of_int i, return (Identifier ("this."^n))) fields),
               Some (return (Identifier "OCamlUnit.WRAPPER")))] in
  let class_type = Reference (class_name, []) in
  let ident_v = Identifier "v" in
  let wrap_meth =
    let formal, effective =
      List.split
        (List.map
           (fun (_, ty, name, _) ->
             (ty, name), Identifier name)
           fields) in
    let generics =
      List.map (fun (x, _) -> x ^ " extends OCamlValue") type_parameters in
    method_
      ~generics
      ~annotations:["@SuppressWarnings(\"unchecked\")"]
      [Public; Static] ~return:(Some class_type) "wrap" ~parameters:(formal @ [type_Value, "v"])
      [return (New (class_name ^ suffix, effective @ [ident_v]))] in
  let wrapper_meth =
    method_
      ~annotations:["@SuppressWarnings(\"unchecked\")"]
      ~generics:(List.map (fun (x, _) -> x ^ " extends OCamlValue") type_parameters)
      [Public; Static] ~return:(Some wrapper_type) "wrapper"
      ~parameters:(List.map (fun (_, ty, name, _) -> ty, name) fields)
      [return (Anonymous_class (wrapper_type',
                                (List.map (fun (x, _) -> Identifier ("w" ^ x)) type_parameters),
                                wrapper_type'0,
                                "wrap",
                                [type_Value, "v"],
                                (New (class_name ^ suffix, (List.map (fun (_, _, name, _) -> Identifier name) fields) @ [Identifier "v"]))))] in
  fields, get_wrapper_meth, get_wrapper_idx_meth, wrapper_meth, wrap_meth

let make_wrapper_elements class_name ?(suffix = "") type_parameters =
  if type_parameters = [] then
    make_wrapper_elements_no_type_parameter class_name suffix
  else
    make_wrapper_elements_type_parameter class_name suffix type_parameters

let make_wrapper_cstr_elements wrapper_fields type_parameters =
  let open JavaAST in
  if type_parameters = [] then
    [], []
  else
    List.split
      (List.map
         (fun (_, ty, name, _) ->
           (ty, name), Assign ("this." ^ name, Identifier name))
         wrapper_fields)

let primitive_type = function
  | JavaAST.Reference _ -> false
  | _ -> true

let is_arrow type_expr =
  let open Types in
  match type_expr.desc with
  | Tarrow _ -> true
  | _ -> false

let flatten_arrow type_expr =
  let open Types in
  let rec flatten acc = function
    | { desc = Types.Tarrow (_, t1, t2, _); _ } -> flatten (t1 :: acc) t2
    | t -> t, List.rev acc in
  match type_expr.desc with
  | Tarrow (_, { desc = (Ttuple l); _ }, t2, _) when not (is_arrow t2) ->
      (* tuplified function *)
      t2, l
  | _ ->
      (* currified function *)
      flatten [] type_expr

let flatten_arrow_not_tuple type_expr =
  let open Types in
  let rec flatten acc = function
    | { desc = Types.Tarrow (_, t1, t2, _); _ } -> flatten (t1 :: acc) t2
    | t -> t, List.rev acc in
  flatten [] type_expr

let same_item x y =
  let open Types in
  match x, y with
  | Sig_type (id, { type_kind = Type_abstract; _ }, _),
    Sig_type (id', { type_kind = Type_abstract;
                     type_manifest = Some { desc = Tconstr (path, _, _); _ }; _ }, _)
    when (Ident.name id) = (Ident.name id') ->
      true, [id, path]
  | Sig_value (id, _), Sig_value (id', _)
  | Sig_type (id, _, _), Sig_type (id', _, _)
  | Sig_exception (id, _), Sig_exception (id', _)
  | Sig_module (id, _, _), Sig_module (id', _, _)
  | Sig_modtype (id, _), Sig_modtype (id', _)
  | Sig_class (id, _, _), Sig_class (id', _, _)
  | Sig_class_type (id, _, _), Sig_class_type (id', _, _) ->
      ((Ident.name id) = (Ident.name id')), []
  | _ -> false, []

let flatten_functor module_types module_type =
  let open Types in
  let rec flatten acc = function
    | Mty_functor (id, Mty_ident path, tl) ->
        flatten ((id, path) :: acc) tl
    | Mty_functor _ -> fail Cannot_determine_functor_signature
    | Mty_ident path -> path, List.rev acc, []
    | Mty_signature s ->
        let same x y =
          if (List.length x) = (List.length y) then
            List.fold_left2
              (fun (acc_same, acc_eq) elem1 elem2 ->
                let same, eq = same_item elem1 elem2 in
                acc_same && same, eq :: acc_eq)
              (true, [])
              x
              y
          else
            false, [] in
        let candidates =
          List.map
            (fun (id, sign, _) ->
              let same, eqs = same sign s in
              if same then
                Some (id, List.flatten eqs)
              else
                None)
            module_types in
        let candidates = map_option candidates in
        match candidates with
        | [id, eqs] ->
            let path = Path.Pident id in
            let eqs =
              List.map
                (fun (id, path) ->
                  match path with
                  | Path.Pdot (Path.Pident module_id, type_id, _) ->
                      if List.exists (fun (id, _) -> (Ident.name id) = (Ident.name module_id)) acc then
                        Ident.name id,
                        Ident.name module_id,
                        type_id
                      else
                        fail Cannot_determine_functor_signature
                  | _ -> fail Cannot_determine_functor_signature)
                eqs in
            path, List.rev acc, eqs
        | _ :: _ :: _ -> fail Cannot_determine_functor_signature
        | [] -> fail Cannot_determine_functor_signature in
  flatten [] module_type

let is_unit t =
  Ctype.moregeneral !State.environment false Predef.type_unit t

let cast_if_needed t e =
  let open JavaAST in
  match e with
  | Cast _ -> e
  | _ ->
      begin match t with
      | Reference (_, _ :: _) -> Cast (t, e)
      | _ -> e
      end

let not_an_object = function
  | Some type_expr ->
      let open Types in
      begin match type_expr.desc with
      | Tobject _ -> false
      | _ -> true
      end
  | None -> true

let make_basic_object_methods name field reference_comparison =
  let open JavaAST in
  let ident_this_value =
    if field <> "" then
      Identifier ("this." ^ field)
    else
      Identifier "super" in
  let ident_that_value =
    if field <> "" then
      Identifier ("that." ^ field)
    else
      Identifier "that" in
  let hash_code_meth =
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some Int) "hashCode"
      [return (Call (ident_this_value, "hashCode", []))] in
  let equals_meth =
    let comp =
      if reference_comparison then
        Infix ("==", ident_this_value, ident_that_value)
      else
        Call (ident_this_value, "equals", [ident_that_value]) in
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some Boolean) "equals" ~parameters:[type_Object, "obj"]
      (make_equals_body_expr_list name "obj" [comp]) in
  let to_string_meth =
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some type_String) "toString"
      [return (String_literal (name ^ "(...)"))] in
  hash_code_meth, equals_meth, to_string_meth
