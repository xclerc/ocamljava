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


module HashedPath = struct
  type t = Path.t
  let equal x y = Path.same x y
  let rec hash = function
    | Path.Pident id -> Hashtbl.hash (Ident.name id)
    | Path.Pdot (p, s, _) -> (hash p) + (Hashtbl.hash s)
    | Path.Papply (p1, p2) -> (hash p1) + (hash p2)
end

module Definitions = Hashtbl.Make (HashedPath)

type definitions = (TypeInfo.t * bool) Definitions.t

let defined : definitions = Definitions.create 17

let locally_defined :definitions = Definitions.create 17

let is_defined_and_doesnt_expand path =
  try
    snd (Definitions.find defined path)
  with Not_found ->
    try
      snd (Definitions.find locally_defined path)
    with _ -> false

let add_local ident dont_expand =
  let path, info = TypeInfo.make_simple ident in
  Definitions.add locally_defined path (info, dont_expand)

let promote modname =
  let modpath = Path.Pident (Ident.create_persistent modname) in
  Definitions.iter
    (fun path (info, dont_expand) ->
      let otyp = Path.Pdot (modpath, Path.name path, 0) in
      let jtyp, name = match info.TypeInfo.java_type with
      | JavaAST.Reference (cn, l) ->
          let class_name = Args.class_of_module modname in
          let name = class_name ^ "." ^ (JavaAST.checked_name cn) in
          JavaAST.Reference (name, l), name
      | x ->
          x, "class" in
      let info = { TypeInfo.ocaml_type = Some otyp;
                   java_type = jtyp;
                   java_of_ocaml = (fun e -> JavaAST.Static_call (name, "wrap", [e]));
                   ocaml_of_java = (fun e -> JavaAST.Call (e, "value", [])); } in
      Definitions.add defined otyp (info, dont_expand))
    locally_defined;
  Definitions.clear locally_defined

let find_predefined boxed path =
  List.find
    (fun x ->
      match x.TypeInfo.ocaml_type with
      | Some p -> Path.same p path
      | None -> false)
    (TypeInfo.predefined_types boxed)
    
let find_defined path =
  try
    try
      fst (Definitions.find defined path)
    with
      Not_found ->
        fst (Definitions.find locally_defined path)
  with Not_found ->
    (* the type is neither in defined, nor in locally_defined
       BUT the cmi file is well-typed
       THUS the type we are looking for is either
       (i) from another module, or
       (ii) from a recursive definition *)
    (match path with
    | Path.Pident id ->
        (* case (i), we return a "fake" record as will be
           build by add_simple when the type is registered *)
        snd (TypeInfo.make_simple id)
    | _ ->
        (* case (ii), the user should have provided the
           other file on the command line *)
        raise Not_found)

let find_from_constructor boxed path =
  try
    find_predefined boxed path
  with Not_found ->
    find_defined path

let rec make_wrapper generics i =
  match i with
  | JavaAST.Reference (cn, l) ->
      if TypeParametersTable.mem cn generics then begin
        match TypeParametersTable.find cn generics with
        | TypeParametersTable.Parameter path ->
            let path = List.rev path in
            begin match path with
            | param :: accesses -> (* appears in parameter *)
                let base = JavaAST.Identifier (Printf.sprintf "p%d" param) in
                List.fold_left
                  (fun acc elem ->
                    JavaAST.Call (acc,
                                  "getWrapper",
                                  [JavaAST.Int_literal (Int32.of_int elem)]))
                  base
                  accesses
            | [] -> assert false
            end
        | TypeParametersTable.Field id -> JavaAST.Identifier id
        | TypeParametersTable.Nowhere -> (* appears only in result *)
            JavaAST.Identifier (Printf.sprintf "w%s" cn)
      end else
        JavaAST.Static_call (cn, "wrapper", List.map (make_wrapper generics) l)
  | _ -> assert false

let augment_with_type_parameters generics res l =
  let l'' = List.map (fun x -> x.TypeInfo.java_type) l in
  let jt = match res.TypeInfo.java_type with
  | JavaAST.Reference (cn, []) -> JavaAST.Reference (cn, l'')
  | _ -> assert false in
  let joo e =
    let l = List.map (fun i -> make_wrapper generics i.TypeInfo.java_type) l in
    match res.TypeInfo.java_of_ocaml e with
    | JavaAST.Static_call (cn, "wrap", [e]) ->
        let tmp = JavaAST.Static_call (cn, "wrap", l @ [e]) in
        tmp
    | _ -> assert false in
  { res with TypeInfo.java_type = jt; java_of_ocaml = joo }

let rec string_of_type_expr te =
  string_of_type_desc te.Types.desc
and string_of_type_desc td =
  let open Types in
  match td with
  | Tvar None -> "'?"
  | Tvar (Some s) -> "'" ^ s
  | Tarrow (_, te1, te2, _) -> (string_of_type_expr te1) ^ " -> " ^ (string_of_type_expr te2)
  | Ttuple l -> string_of_type_expr_list l
  | Tconstr (p, [], _) -> Path.name p
  | Tconstr (p, l, _) -> (string_of_type_expr_list l) ^ " " ^ (Path.name p)
  | Tobject _ -> "< .. >"
  | Tfield (s, _, _, _) -> s
  | Tnil -> "nil"
  | Tlink te -> string_of_type_expr te
  | Tsubst te -> string_of_type_expr te
  | Tvariant { row_fields; _ } ->
      let string_of_row_field (lbl, _) = lbl in
      "[" ^ (String.concat " | " (List.map string_of_row_field row_fields)) ^ "]"
  | Tunivar None -> "'?"
  | Tunivar (Some s) -> "'" ^ s
  | Tpoly (te, tl) -> (string_of_type_expr te) ^ " . " ^ (string_of_type_expr_list tl)
  | Tpackage (p, _, _) -> "(module " ^ (Path.name p) ^ ")"
and string_of_type_expr_list l =
  String.concat " * " (List.map string_of_type_expr l)

let rec find ?(generics = TypeParametersTable.empty) boxed t =
  let open Types in
  match t.desc with
  | Tlink t' -> find ~generics boxed t'
  | Tobject (_, { contents = Some (path, l) }) | Tconstr (path, l, _)
    when is_defined_and_doesnt_expand path ->
      (* before calling Ctype.{full_expand,repr} because we need the path *)
      let res = find_defined path in
      let l = List.map (find ~generics true) l in
      augment_with_type_parameters generics res l
  | _ ->
  let t =
    try
      Ctype.full_expand !State.environment (Ctype.repr t)
    with _ -> t in
  match t.desc with
  | Tlink t' -> find ~generics boxed t'
  | Tvar (Some id) when TypeParametersTable.mem id generics ->
      { TypeInfo.ocaml_type = None;
        java_type = JavaAST.Reference (id, []);
        ocaml_of_java = (fun e -> JavaAST.Call (e, "value", []));
        java_of_ocaml = (fun e -> JavaAST.Static_call (Printf.sprintf "w%s" id, "wrap", [e])); }
  | Tvar _ | Tunivar _ ->
      { TypeInfo.ocaml_type = None;
        java_type = JavaAST.Reference ("OCamlValue", []);
        ocaml_of_java = (fun e -> JavaAST.Call (e, "value", []));
        java_of_ocaml = (fun e -> JavaAST.Static_call ("OCamlValue", "wrap", [e])); }
  | Ttuple l ->
      if (List.length l) > 8 then
        Wrap_common.(fail (Tuple_is_too_large (List.length l)));
      let class_name = Printf.sprintf "OCamlTuple%d" (List.length l) in
      let l = List.map (find ~generics true) l in
      let java_of_ocaml e =
        let l = List.map (fun i -> make_wrapper generics i.TypeInfo.java_type) l in
        let tmp = JavaAST.Static_call (class_name, "wrap", l @ [e]) in
        if l = [] then
          tmp
        else
          JavaAST.Cast (JavaAST.Reference (class_name, []), tmp) in
      let ocaml_of_java e =
        JavaAST.Call (e, "value", []) in
      let java_type =
        JavaAST.Reference (class_name, (List.map (fun x -> x.TypeInfo.java_type) l)) in
      { TypeInfo.ocaml_type = None;
        java_type;
        java_of_ocaml;
        ocaml_of_java; }
  | Tconstr (path, [], _) ->
      find_from_constructor boxed path
  | Tconstr (path, l, _) ->
      let res = find_from_constructor boxed path in
      let l = List.map (find ~generics true) l in
      augment_with_type_parameters generics res l
  | Tarrow _ ->
      let return, parameters = Wrap_common.flatten_arrow_not_tuple t in
      let return_type = find ~generics true return in
      let parameter_types = List.map (find ~generics true) parameters in
      let wrappers =
        List.map
          (fun param ->
            let w = make_wrapper generics param.TypeInfo.java_type in
            let c = JavaAST.Reference ("Wrapper", [param.TypeInfo.java_type]) in
            JavaAST.Cast (c, w))
          parameter_types in
      let class_name =
        match List.length parameters with
        | 1 -> "OCamlFunction"
        | (2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10) as x -> Printf.sprintf "OCamlFunction%d" x
        | n -> Wrap_common.(fail (Function_arity_is_too_large n)) in
      let java_type =
        JavaAST.Reference (class_name,
                           ((List.map (fun x -> x.TypeInfo.java_type) parameter_types)
                            @ [return_type.TypeInfo.java_type])) in
      let wrap e =
        let rec mk_wrapper java_type =
          match java_type with
          | JavaAST.Reference (cn, l) ->
              JavaAST.Static_call (cn, "wrapper", (List.map mk_wrapper l))
          | _ -> assert false in
        let wrappers =
          List.map
            (fun param -> mk_wrapper param.TypeInfo.java_type)
            (parameter_types @ [return_type]) in
        let res = JavaAST.Static_call (class_name, "wrap", wrappers @ [e]) in
        JavaAST.Cast (java_type, res) in
      { TypeInfo.ocaml_type = None;
        java_type = java_type;
        java_of_ocaml = wrap;
        ocaml_of_java = (fun e -> JavaAST.Call (e, "getClosure", wrappers)); }
  | _ -> Wrap_common.(fail (Cannot_map_type (string_of_type_expr t)))

let find ?(generics = TypeParametersTable.empty) boxed t =
  try
    find ~generics boxed t
  with Not_found ->
    Wrap_common.(fail (Cannot_find_type (string_of_type_expr t)))
