(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java optimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java optimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open BaristaLibrary


type remove_index_function = BaristaLibrary.Name.for_class -> int -> bool

let make_class_name_ext s =
  s
  |> UTF8.of_string
  |> Name.make_for_class_from_external

let make_method_name s =
  s
  |> UTF8.of_string
  |> Name.make_for_method

let make_field_name s =
  s
  |> UTF8.of_string
  |> Name.make_for_field

let global_class_of_class cn =
  UTF8.(Name.internal_utf8_for_class cn ++ (of_string "$Global"))
  |> Name.make_for_class_from_internal

let runtime_prefix = UTF8.of_string "org.ocamljava.runtime."

let is_runtime_class cn =
  cn
  |> Name.external_utf8_for_class
  |> UTF8.starts_with runtime_prefix

let shared_constant_prefixes =
  [ UTF8.of_string "INT_" ;
    UTF8.of_string "INT32_" ;
    UTF8.of_string "INT64_" ;
    UTF8.of_string "NATIVEINT_" ;
    UTF8.of_string "FLOAT_" ]

let is_shared_constant fn =
  let fn = Name.utf8_for_field fn in
  List.exists
    (fun p -> UTF8.starts_with p fn)
    shared_constant_prefixes
