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

let wrap modname name res params eqs module_types approx approx_idx =
  let fname = Ident.name name in
  Output.verbose (Printf.sprintf "  wrapping functor %S..." fname);
  let open JavaAST in
  let links = ref [] in
  let meth_parameters =
    List.mapi
      (fun i (mn, mt) ->
        let id = Printf.sprintf "p%d" i in
        let t = Path.name mt in
        let _, _, l =
          List.find
            (fun (id, _, _) -> (Ident.name id) = t)
            module_types in
        let mn = Ident.name mn in
        let l =
          List.mapi
            (fun i x ->
              try
                let idx_type = ref (-1) in
                let link, _, _ =
                  List.find
                    (fun (_, parameter, typ) ->
                      incr idx_type; (parameter = mn) && (typ = x))
                    eqs in
                links := (link, (i, !idx_type)) :: !links;
                Reference (link, [])
              with Not_found ->
                Reference (x, []))
            l in
        let t = Reference (t, l) in
        let formal = t, id in
        let effective = Identifier id in
        let effective = Call (effective, "value", []) in
        formal, effective)
      params in
  let formal_meth_parameters, effective_meth_parameters =
    List.split meth_parameters in
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
      effective_meth_parameters @ [get_global approx_idx] in
  let call = Static_call (class_name, func_name, effective_meth_parameters) in
  let _, _, res_type_params =
    List.find
      (fun (id, _, _) ->
        (Ident.name id) = (Path.name res))
      module_types in
  let res_type_params, quantif, wrappers =
    List.fold_left
      (fun (acc, quantif, wrappers) elem ->
        if List.mem_assoc elem !links then
          let p_id, w_id = List.assoc elem !links in
          let wrapper = Identifier (Printf.sprintf "p%d" p_id) in
          let wrapper = Call (wrapper, "getWrapper", [Int_literal (Int32.of_int w_id)]) in
          elem :: acc, elem :: quantif, wrapper :: wrappers
        else
          let wrapper = Identifier "OCamlValue.WRAPPER" in
          "OCamlValue" :: acc, quantif, wrapper :: wrappers)
      ([], [], [])
      res_type_params in
  let res_type_params = List.rev res_type_params in
  let quantif = List.rev quantif in
  let wrappers = List.rev wrappers in
  let res_type_params = List.map (fun x -> Reference (x, [])) res_type_params in
  let res_type = Reference (Path.name res, res_type_params) in
  let res_conv e =
    Static_call (Path.name res, "wrap", wrappers @ [e]) in
  let meth_return_type = Some (res_type, res_conv, false) in
  let type_parameters = TypeParametersTable.empty in
  let body = Wrap_arrow.make_body meth_return_type type_parameters call in
  let body = Wrap_arrow.make_try_catch body in
  let javadoc =
    [ Printf.sprintf "Applies functor {@code %s.%s}." modname fname ] in
  method_
    ~javadoc
    ~annotations:["@SuppressWarnings(\"unchecked\")"]
    ~generics:(List.map (Printf.sprintf "%s extends OCamlValue") quantif)
    [ Public; Static ]
    ~return:(match meth_return_type with None -> None | Some (x, _, _) -> Some x)
    fname
    ~parameters:(formal_meth_parameters (*@ additional_parameters*))
    ~throws:Wrap_arrow.throws_list
    body
