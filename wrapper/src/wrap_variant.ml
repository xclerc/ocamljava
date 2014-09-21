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

type variant_kind =
  | Simple_variant
  | Polymorphic_variant

let get_element variant_kind x fields idx =
  if variant_kind = Simple_variant then
    get x idx
  else if (List.length fields) = 1 then
    get x 1
  else
    get (get x 1) idx

let wrap ident constructors variant_kind type_parameters =
  let open JavaAST in
  let name = Ident.name ident in
  TypeMap.add_local ident (variant_kind = Polymorphic_variant);
  let generics = TypeParametersTable.make type_parameters in
  let wrapper_fields, get_wrapper_meth, get_wrapper_idx_meth, wrapper_meth, wrap_meth =
    Wrap_common.make_wrapper_elements name type_parameters in
  let class_type = Reference (name, []) in
  let type_T = Reference ("T", []) in
  let type_Visitor_T =
    Reference ("Visitor",
               type_T :: (List.map (fun (id, _) -> Reference (id, [])) type_parameters)) in
  let ident_v = Identifier "v" in
  let ident_this = Identifier "this" in
  let ident_this_value = Identifier "this.value" in
  let ident_that_value = Identifier "that.value" in
  let type_TAG = Reference ("TAG", []) in
  let cstr =
    let wrapper_parameters, inits =
      Wrap_common.make_wrapper_cstr_elements wrapper_fields type_parameters in
    constructor
      [Private] name ~parameters:(wrapper_parameters @ [type_Value, "v"])
      ([Super_constructor [ident_v]] @ inits) in
  let make_switch_block long_block mk_block def_block =
    let _, block_cases =
      List.fold_left
        (fun (idx_block, block_cases) (id, fields) ->
          if fields = [] then begin
            idx_block, block_cases
          end else begin
            let case = (idx_block, mk_block idx_block id fields) in
            Int32.succ idx_block, case :: block_cases
          end)
        (0l, [])
        constructors in
    let if_block =
      Switch (Call (ident_this_value, "getTag", []), List.rev block_cases, def_block) in
    If_else (Call (ident_this_value, "isBlock", []), if_block, long_block) in
  let make_switch_simple mk_long mk_block def_long def_block =
    let _, block_cases, _, long_cases =
      List.fold_left
        (fun (idx_block, block_cases, idx_long, long_cases) (id, fields) ->
          if fields = [] then begin
            let case = (idx_long, mk_long idx_long id) in
            idx_block, block_cases, Int32.succ idx_long, case :: long_cases
          end else begin
            let case = (idx_block, mk_block idx_block id fields) in
            Int32.succ idx_block, case :: block_cases, idx_long, long_cases
          end)
        (0l, [], 0l, [])
        constructors in
    let if_block =
      Switch (Call (ident_this_value, "getTag", []), List.rev block_cases, def_block) in
    let else_block =
      Switch (Call (ident_this_value, "asCastedInt", []), List.rev long_cases, def_long) in
    If_else (Call (ident_this_value, "isBlock", []), if_block, else_block) in
  let make_switch_poly mk_long mk_block def_long def_block =
    let block_cases, long_cases =
      List.fold_left
        (fun (block_cases, long_cases) (id, fields) ->
          let tag = Btype.hash_variant id in
          let tag = Int32.of_int tag in
          if fields = [] then begin
            let case = (tag, mk_long id) in
            block_cases, case :: long_cases
          end else begin
            let case = (tag, mk_block id fields) in
            case :: block_cases, long_cases
          end)
        ([], [])
        constructors in
    let if_block =
      Switch (Call (get ident_this_value 0, "asCastedInt", []), List.rev block_cases, def_block) in
    let else_block =
      Switch (Call (ident_this_value, "asCastedInt", []), List.rev long_cases, def_long) in
    If_else (Call (ident_this_value, "isBlock", []), if_block, else_block) in
  let tag_meth =
    let default = Some (return Null_literal) in
    let ident id = return (Identifier (Printf.sprintf "TAG.%s" id)) in
    if variant_kind = Simple_variant then
      method_
        [Public] ~return:(Some type_TAG) "tag"
        [make_switch_simple
           (fun _ id -> ident id)
           (fun _ id _ -> ident id)
           default
           default]
    else
      method_
        [Public] ~return:(Some type_TAG) "tag"
        [make_switch_poly
           (fun id -> ident id)
           (fun id _ -> ident id)
           default
           default] in
  let accessor_meths =
    List.map
      (fun (id, fields) ->
        List.mapi
          (fun idx field ->
            let info = TypeMap.find ~generics false field in
            let res = get_element variant_kind ident_this_value fields idx in
            let res = info.TypeInfo.java_of_ocaml res in
            let name = Printf.sprintf "get%s%d" id idx in
            method_
              [Public] ~return:(Some info.TypeInfo.java_type) name
              [return (cast_if_needed info.TypeInfo.java_type res)])
          fields)
      constructors in
  let visit_meth =
    let default = Some (return Null_literal) in
    let ident_visitor = Identifier "visitor" in
    if variant_kind = Simple_variant then
        method_
        ~generics:["T"]
        [Public] ~return:(Some type_T) "visit" ~parameters:[type_Visitor_T, "visitor"]
        [make_switch_simple
           (fun _ id ->
           return (Call (ident_visitor, "visit" ^ id, [])))
           (fun _ id fields ->
             let l =
               List.mapi
                 (fun i field ->
                   let info = TypeMap.find ~generics false field in
                   let v = get ident_this_value i in
                   let v = info.TypeInfo.java_of_ocaml v in
                   cast_if_needed info.TypeInfo.java_type v)
                 fields in
             return (Call (ident_visitor, "visit" ^ id, l)))
           default
           default]
    else
      method_
        ~generics:["T"]
        [Public] ~return:(Some type_T) "visit" ~parameters:[type_Visitor_T, "visitor"]
        [make_switch_poly
           (fun id ->
           return (Call (ident_visitor, "visit" ^ id, [])))
           (fun id fields ->
             let l =
               List.mapi
                 (fun i field ->
                   let info = TypeMap.find ~generics false field in
                   let v = get_element variant_kind ident_this_value fields i in
                   let v = info.TypeInfo.java_of_ocaml v in
                   cast_if_needed info.TypeInfo.java_type v)
                 fields in
             return (Call (ident_visitor, "visit" ^ id, l)))
           default
           default] in
  let hash_code_meth =
    let call = Call (ident_this, "tag", []) in
    let call = Call (call, "hashCode", []) in
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some Int) "hashCode"
      [return call] in
  let equals_meth =
    let default = Some (return (Boolean_literal false)) in
    let body =
      if variant_kind = Simple_variant then
        make_switch_block
          (return (Infix ("==",
                          Call (ident_that_value, "asLong", []),
                          Call (ident_that_value, "asLong", []))))
          (fun _ _ fields ->
            let l =
              List.mapi
                (fun idx field ->
                  let info = TypeMap.find ~generics false field in
                  let x = get ident_this_value idx in
                  let x = info.TypeInfo.java_of_ocaml x in
                  let y = get ident_that_value idx in
                  let y = info.TypeInfo.java_of_ocaml y in
                  if primitive_type info.TypeInfo.java_type then
                    Infix ("==", x, y)
                  else
                    Call (x, "equals", [y]))
                fields in
            return (and_list l))
          default
      else
        make_switch_block
          (return (Infix ("==",
                          Call (ident_that_value, "asLong", []),
                          Call (ident_that_value, "asLong", []))))
          (fun _ _ fields ->
            let l =
              List.mapi
                (fun idx field ->
                  let info = TypeMap.find ~generics false field in
                  let x = get_element variant_kind ident_this_value fields idx in
                  let x = info.TypeInfo.java_of_ocaml x in
                  let y = get_element variant_kind ident_that_value fields idx in
                  let y = info.TypeInfo.java_of_ocaml y in
                  if primitive_type info.TypeInfo.java_type then
                    Infix ("==", x, y)
                  else
                    Call (x, "equals", [y]))
                fields in
            return (and_list l))
          default in
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some Boolean) "equals" ~parameters:[type_Object, "obj"]
      (make_equals_body name "obj" [body]) in
  let to_string_meth =
    let type_string_builder = Reference ("StringBuilder", []) in
    let append e = Expression (Call (Identifier "sb", "append", [e])) in
    let append_string s = append (String_literal s) in
    let default = Some (return Null_literal) in
    let long_case id =
      return (String_literal (Printf.sprintf "%s.%s()" name id)) in
    let block_case id fields =
      Block
        ([ Variable_declaration (type_string_builder, "sb", New ("StringBuilder", []));
           append_string (Printf.sprintf "%s.%s(" name id) ]
         @ (List.flatten (List.mapi
                            (fun idx field ->
                              let info = TypeMap.find ~generics false field in
                              let v = get_element variant_kind ident_this_value fields idx in
                              let v = info.TypeInfo.java_of_ocaml v in
                              if idx > 0 then
                                [append_string ", "; append v]
                              else
                                [append v])
                            fields))
         @ [ append_string ")";
             return (Call (Identifier "sb", "toString", [])) ]) in
    let body =
      if variant_kind = Simple_variant then
        make_switch_simple
          (fun _ id -> long_case id)
          (fun _ id fields -> block_case id fields)
          default
          default
      else
        make_switch_poly
          (fun id -> long_case id)
          (fun id fields -> block_case id fields)
          default
          default in
    method_
      ~annotations:["@Override"]
      [Public] ~return:(Some type_String) "toString"
      [body] in
  let _, _, create_meths =
    List.fold_left
      (fun (idx_block, idx_long, acc) (id, fields) ->
        if fields <> [] then begin
          let l =
            List.mapi
              (fun i field ->
                let p = Identifier (Printf.sprintf "p%d" i) in
                let info = TypeMap.find ~generics false field in
                info.TypeInfo.ocaml_of_java p)
              fields in
          let old_name = name in
          let name = Printf.sprintf "create%s" id in
          let used = ref [] in
          let parameters =
            List.mapi
              (fun i field ->
                let info = TypeMap.find ~generics false field in
                (match field.Types.desc with
                | Types.Tvar (Some id) ->
                    if not (List.mem_assoc id !used) then begin
                      used := (id, i) :: !used
                    end
                | _ -> ());
                info.TypeInfo.java_type, Printf.sprintf "p%d" i)
              fields in
          let meth =
            let additional_parameters = ref [] in
            let quantif =
              List.map
                (fun (id, _) ->
                  if not (List.mem_assoc id !used) then begin
                    let ap = Reference ("Wrapper", [Reference (id, [])]), "w" ^ id in
                    additional_parameters := ap :: !additional_parameters;
                  end;
                  Printf.sprintf "%s extends OCamlValue" id)
                type_parameters in
            let parameters = parameters @ !additional_parameters in
            let wrappers =
              List.map
                (fun (id, _) ->
                  if List.mem_assoc id !used then begin
                    let p = Identifier ("p" ^ (string_of_int (List.assoc id !used))) in
                    Call (p, "getWrapper", [])
                  end else begin
                    Identifier ("w" ^ id)
                  end)
                type_parameters in
            let res =
              if variant_kind = Simple_variant then
                New (old_name, wrappers @ [create_block idx_block l])
              else if (List.length fields) = 1 then
                let tag = Btype.hash_variant id in
                New (old_name, wrappers @ [create_block 0 ((create_long tag) :: l)])
              else
                let tag = Btype.hash_variant id in
                let params = create_block 0 l in
                New (old_name, wrappers @ [create_block 0 ((create_long tag) :: [params])]) in
            method_
              ~generics:quantif
              ~annotations:(if quantif = [] then [] else ["@SuppressWarnings(\"unchecked\")"])
              [Public; Static] ~return:(Some class_type) name ~parameters
              [return res] in
          (succ idx_block, idx_long, meth :: acc)
        end else begin
          let res =
            if variant_kind = Simple_variant then
              New (name, [create_long idx_long])
            else
              New (name, [create_long (Btype.hash_variant id)]) in
          let name = Printf.sprintf "create%s" id in
          let meth =
            method_
              [Public; Static] ~return:(Some class_type) name
              [return res] in
          (idx_block, succ idx_long, meth :: acc)
        end)
      (0, 0, [])
      constructors in
  let tag_class =
    { enum_modifiers = [Public; Static];
      enum_name = "TAG";
      enum_values = (List.map fst constructors); } in
  let visitor_itf =
    let meths =
      List.map
        (fun (id, fields) ->
          let l =
            List.map
              (fun field ->
                let info = TypeMap.find ~generics false field in
                info.TypeInfo.java_type)
              fields in
          type_T, "visit" ^ id, l)
        constructors in
    let l = List.map fst type_parameters in
    let l = String.concat ", " l in
    let l = if l = "" then l else ", " ^ l in
    { interf_modifiers = [Public];
      interf_name = "Visitor<T" ^ l ^ ">";
      interf_methods = meths; } in
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
    ~methods:([cstr;
               get_wrapper_meth;
               get_wrapper_idx_meth;
               tag_meth] @
              (List.flatten accessor_meths) @
              [visit_meth;
               hash_code_meth;
               equals_meth;
               to_string_meth] @
              (List.rev create_meths) @
              [wrap_meth;
               wrapper_meth])
    ~inner:[Enum tag_class;
            Interface visitor_itf]
    ()
