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


let wrap ident type_parameters =
  let open JavaAST in
  let name = Ident.name ident in
  TypeMap.add_local ident false;
  let wrapper_fields, get_wrapper_meth, get_wrapper_idx_meth, wrapper_meth, wrap_meth =
    Wrap_common.make_wrapper_elements name type_parameters in
  let ident_v = Identifier "v" in
  let cstr =
    let wrapper_parameters, inits =
      Wrap_common.make_wrapper_cstr_elements wrapper_fields type_parameters in
    constructor
      [Private] name ~parameters:(wrapper_parameters @ [type_Value, "v"])
      ([Super_constructor [ident_v]] @ inits) in
  let hash_code_meth, equals_meth, to_string_meth =
    Wrap_common.make_basic_object_methods name "value" true in
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
    [Public; Static; Final]
    full_name
    ~extends:(Some "OCamlValue")
    ~fields:wrapper_fields
    ~methods:[cstr;
              get_wrapper_meth;
              get_wrapper_idx_meth;
              hash_code_meth;
              equals_meth;
              to_string_meth;
              wrap_meth;
              wrapper_meth] ()

