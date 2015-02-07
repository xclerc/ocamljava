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

let wrap modname ident type_expr global_idx =
  let name = Ident.name ident in
  Output.verbose (Printf.sprintf "  wrapping value %S..." name);
  let open JavaAST in
  let meth_javadoc =
    Printf.sprintf "Returns the value of {@code %s.%s}." modname name in
  let info = TypeMap.find false type_expr in
  let type_needs_cast =
    match info.TypeInfo.java_type with
    | Reference (_, _ :: _) -> true
    | _ -> false in
  let value = get_global global_idx in
  let res = info.TypeInfo.java_of_ocaml (Identifier "res") in
  let needs_cast = match res with Cast _ -> false | _ -> type_needs_cast in
  let res =
    if needs_cast then
      Cast (info.TypeInfo.java_type, res)
    else
      res in
  method_
    ~javadoc:[meth_javadoc]
    [Public; Static] ~return:(Some info.TypeInfo.java_type) name
    [Variable_declaration (type_Value, "res", value);
     return res]
