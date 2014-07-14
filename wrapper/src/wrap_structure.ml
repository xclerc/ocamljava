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

let make_try_catch body =
  let open JavaAST in
  let handlers = [
    "OCamlException", "oe",
    [ Throw (Call (Identifier "oe", "getFailException", [])) ];
  ] in
  let body = [Try_catch (body, handlers)] in
  body

(* "reverse" of Wrap_arrow version *)
let make_body meth_return_type call =
  let open JavaAST in
  match meth_return_type with
  | Some (t, conv, _gen) ->
      let ret =
          return (cast_if_needed t (conv (Identifier "res"))) in
      [ Variable_declaration (t, "res", call) ;
        ret ]
  | None -> [ Expression call; return (Identifier "Value.UNIT") ]

let rec split3 = function
  | (x, y, z) :: tl ->
      let x', y', z' = split3 tl in
      x :: x', y :: y', z :: z'
  | [] -> [], [], []

let make_methods is_object methods =
  let open JavaAST in
  let name_hash =
    List.map
      (fun (name, _, _) -> Btype.hash_variant name, name)
      methods in
  let name_hash = List.sort Pervasives.compare name_hash in
  let name_hash = List.mapi (fun i (_, x) -> x, i) name_hash in
  let all_methods =
    List.map
      (fun (name, type_expr, index) ->
        let meth_return_type, meth_parameters = Wrap_common.flatten_arrow type_expr in
        let type_parameters =
          Wrap_arrow.get_type_parameters meth_parameters meth_return_type in
        let meth_return_type =
          Wrap_arrow.return_type_of_type_expr
            ~reverse:true type_parameters meth_return_type
        and meth_return_type' =
          Wrap_arrow.return_type_of_type_expr
            ~reverse:false type_parameters meth_return_type in
        let return =
          match meth_return_type with
          | Some (x, _, _) -> Some x
          | None -> None in
        let both_parameters =
          List.mapi
            (fun i te ->
              if is_unit te then
                None, Identifier "Value.UNIT"
              else
                let id = Printf.sprintf "p%d" i in
                let tt = TypeMap.find ~generics:type_parameters false te in
                let t = tt.TypeInfo.java_type in
                let x = tt.TypeInfo.ocaml_of_java (Identifier id) in
                Some (t, id), x)
            meth_parameters in
        let parameters, parameters2 = List.split both_parameters in
        let parameters = map_option parameters in
        let meth =
          method_
            [Public; Abstract]
            ~return
            name
            ~parameters
            [] in
        let meth2 =
          let this_value = Identifier "this.value" in
          let target =
            match index with
            | Some x -> (get this_value x)
            | None ->
                let idx = List.assoc name name_hash in
                get (get this_value 0) (2 + (2 * idx)) in
          let apply_params =
            if is_object then
              target :: this_value :: parameters2
            else
              target :: parameters2 in
          let call = Static_call ("NativeApply", "apply", apply_params) in
          let body = Wrap_arrow.make_body meth_return_type' type_parameters call in
          let body = Wrap_arrow.make_try_catch ~catch_all:true body in
          method_
            ~annotations:["@Override"]
            [Public; Final]
            ~return
            name
            ~parameters
            body in
        let parameters =
          List.mapi
            (fun i te ->
              let id = Printf.sprintf "p%d" i in
              if is_unit te then
                Some (type_Value, id), None
              else
                let tt = TypeMap.find ~generics:type_parameters false te in
                let t = type_Value in
                let conv = tt.TypeInfo.java_of_ocaml in
                let formal = t, id in
                let effective = conv (Identifier id) in
                Some formal, Some effective)
            meth_parameters in
        let formal_meth_parameters, effective_meth_parameters =
          List.split parameters in
        let formal_meth_parameters = map_option formal_meth_parameters in
        let effective_meth_parameters = map_option effective_meth_parameters in
        let call = Call (Identifier "this", name, effective_meth_parameters) in
        let body = make_body meth_return_type call in
        let body = make_try_catch body in
        let formal_meth_parameters =
          if is_object then
            let dummy_self = type_Value, "self" in
            dummy_self :: formal_meth_parameters
          else
            formal_meth_parameters in
        let closure =
          method_
            [Public; Final] (* public needed by method lookup *)
            ~return:(Some type_Value)
            (name ^ "$impl")
            ~parameters:formal_meth_parameters
            ~throws:["FailException"]
            body in
        (meth, meth2, closure))
      methods in
  split3 all_methods
