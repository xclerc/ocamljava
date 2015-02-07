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
open TypeParametersTable

let throws_list = [
  "OCamlException"
]

let return_type_of_type_expr ?(reverse = false) generics t =
  if is_unit t then
    None
  else
    let gen = (match t.Types.desc with Types.Tvar (Some _) -> true | _ -> false) in
    let t = TypeMap.find ~generics false t in
    let conv =
      if reverse then
        t.TypeInfo.ocaml_of_java
        else
        t.TypeInfo.java_of_ocaml in
    Some (t.TypeInfo.java_type, conv, gen)

let rec expand_if_needed te =
  let open Types in
  match te.desc with
  | Tlink te -> expand_if_needed te
  | Tconstr (path, _, _) when TypeMap.is_defined_and_doesnt_expand path ->
      te
  | Tobject (_, { contents = Some (path, _) })
    when TypeMap.is_defined_and_doesnt_expand path ->
      te
  | _ ->
      try
        Ctype.full_expand !State.environment (Ctype.repr te)
      with _ -> te

let rec visit t path te =
  let open Types in
  match te.desc with
  | Tvar None -> ()
  | Tvar (Some id) ->
      add_parameter t id path
  | Tarrow (_, te1, te2, _) ->
      visit_arrow t path 0 te1 te2
  | Ttuple l ->
      visit_list t path l
  | Tconstr (_path, l, _) ->
      visit_list t path l
  | Tobject _
  | Tfield _
  | Tnil
  | Tsubst _
  | Tvariant _
  | Tunivar _
  | Tpoly _
  | Tpackage _ -> ()
  | Tlink te -> visit t path te
and visit_list t path l =
  List.iteri
    (fun i e ->
      visit t (i :: path) e)
    l
and visit_arrow t path i te1 te2 =
  let open Types in
  visit t (i :: path) te1;
  match te2.desc with
  | Tarrow (_, te1', te2', _) ->
      visit_arrow t path (succ i) te1' te2'
  | _ ->
      visit t ((succ i) :: path) te2

let get_type_parameters_for_params type_expr_list =
  let parameters = ref TypeParametersTable.empty in
  let type_expr_list = List.map expand_if_needed type_expr_list in
  List.iteri (fun i e -> visit parameters [i] e) type_expr_list;
  !parameters

let get_type_parameters_for_return type_expr =
  let parameters = ref TypeParametersTable.empty in
  let type_expr = expand_if_needed type_expr in
  visit parameters [] type_expr;
  !parameters

let merge parameters return =
  TypeParametersTable.fold
    (fun k _ acc ->
      TypeParametersTable.add k TypeParametersTable.Nowhere acc)
    return
    parameters

let get_type_parameters meth_parameters meth_return_type =
  let meth_parameters = get_type_parameters_for_params meth_parameters in
  let meth_return_type = get_type_parameters_for_return meth_return_type in
  let res = merge meth_parameters meth_return_type in
  res

let generics_of_type_parameters type_parameters =
  TypeParametersTable.fold
    (fun k _ acc -> k :: acc)
    type_parameters
    []

let make_body meth_return_type type_parameters call =
  let open JavaAST in
  match meth_return_type with
  | Some (t, conv, gen) ->
      let ret =
        if gen then
          let base = (Identifier "res") in
          let w = TypeMap.make_wrapper type_parameters t in
          let w = Cast (Reference ("Wrapper", [t]), w) in
          return (cast_if_needed t (Call (w, "wrap", [base])))
        else
          return (cast_if_needed t (conv (Identifier "res"))) in
      [ Variable_declaration (Reference ("Value", []), "res", call) ;
        ret ]
  | None -> [ Expression call ]

let make_try_catch ?(catch_all = false) body =
  let open JavaAST in
  let handlers = [
    "FailException", "fe",
    [ Throw (Static_call ("OCamlException", "wrap", [Identifier "fe"])) ];
  ] in
  let handlers =
    if catch_all then
      handlers @ [
                 "Throwable", "t",
                 [ Throw (New ("RuntimeException", [Identifier "t"])) ]
               ]
    else
      handlers in
  let body = [Try_catch (body, handlers)] in
  body

let wrap modname name type_expr approx global_idx =
  Output.verbose (Printf.sprintf "  wrapping function %S..." (Ident.name name));
  let open JavaAST in
  let meth_return_type, meth_parameters = flatten_arrow type_expr in
  let type_parameters = get_type_parameters meth_parameters meth_return_type in
  let generics = generics_of_type_parameters type_parameters in
  let meth_return_type = return_type_of_type_expr type_parameters meth_return_type in
  let meth_parameters =
    List.mapi
      (fun i te ->
        if is_unit te then
          None, JavaAST.Identifier "Value.UNIT"
        else
          let id = Printf.sprintf "p%d" i in
          let tt = TypeMap.find ~generics:type_parameters false te in
          let t = tt.TypeInfo.java_type in
          let conv = tt.TypeInfo.ocaml_of_java in
          let formal = t, id in
          let effective = conv (Identifier id) in
          Some formal, effective)
      meth_parameters in
  let formal_meth_parameters, effective_meth_parameters =
    List.split meth_parameters in
  let formal_meth_parameters = map_option formal_meth_parameters in
  let class_name, func_name, closed =
    match approx with
    | Some (Jlambda.Value_closure (fundesc, _)) ->
        fundesc.Jlambda.fun_label.Jlambda.fl_class,
        fundesc.Jlambda.fun_label.Jlambda.fl_method,
        fundesc.Jlambda.fun_closed
    | _ -> fail (Cannot_determine_function name) in
  let effective_meth_parameters =
    if closed then
      effective_meth_parameters
    else
      effective_meth_parameters @ [get_global global_idx] in
  let call = Static_call (class_name, func_name, effective_meth_parameters) in
  let body =
    make_body meth_return_type type_parameters call
    |> make_try_catch in
  let javadoc =
    [ Printf.sprintf "Calls function {@code %s.%s}." modname (Ident.name name) ] in
  let additional_parameters =
    TypeParametersTable.fold
      (fun k v acc ->
        if v = TypeParametersTable.Nowhere then begin (* appears only in result *)
          let ty = Reference ("Wrapper", [Reference (k, [])]) in
          let id = Printf.sprintf "w%s" k in
          (ty, id) :: acc
        end else
          acc)
      type_parameters
      [] in
  let generics =
    List.map
      (Printf.sprintf "%s extends OCamlValue")
      generics in
  method_
    ~javadoc
    ~generics
    [ Public; Static ]
    ~return:(match meth_return_type with None -> None | Some (x, _, _) -> Some x)
    (Ident.name name)
    ~parameters:(formal_meth_parameters @ additional_parameters)
    ~throws:throws_list
    body

let wrap_closure modname name type_expr approx global_idx =
  let open JavaAST in
  match approx with
  | Some (Jlambda.Value_closure (fundesc, _)) when fundesc.Jlambda.fun_closed ->
      let type_parameters =
        let meth_return_type, meth_parameters = flatten_arrow type_expr in
        get_type_parameters meth_parameters meth_return_type in
      let generics = generics_of_type_parameters type_parameters in
      let javadoc =
        [ Printf.sprintf "Returns closure for function {@code %s.%s}."
            modname (Ident.name name) ] in
      let return_info = TypeMap.find ~generics:type_parameters true type_expr in
      let return_type = Some return_info.TypeInfo.java_type in
      let value = get_global global_idx in
      let closure = return_info.TypeInfo.java_of_ocaml (Identifier "res") in
      let generics =
        List.map
          (Printf.sprintf "%s extends OCamlValue")
          generics in
      let res =
        method_
          ~javadoc
          ~generics
          [ Public; Static ]
          ~return:return_type
          ((Ident.name name) ^ "$closure")
          [Variable_declaration (type_Value, "res", value);
           return closure] in
      Some res
  | _ -> None
