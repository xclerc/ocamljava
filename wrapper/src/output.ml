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


type warning =
  | Discard_unit_value of Ident.t
  | Discard_object_value of Ident.t
  | Discard_polymorphic_variant_value of Ident.t
  | Discard_unsupported_value of Ident.t
  | Discard_unsupported_type of Ident.t
  | Discard_nested_module of Ident.t
  | Discard_object_type of Ident.t

let string_of_warning = function
  | Discard_unit_value id ->
      Printf.sprintf "discarding %S (as it has type \"unit\")"
        (Ident.name id)
  | Discard_object_value id ->
      Printf.sprintf "discarding %S (object type should use class type definition)"
        (Ident.name id)
  | Discard_polymorphic_variant_value id ->
      Printf.sprintf "discarding %S (polymorphic variant should use type definition)"
        (Ident.name id)
  | Discard_unsupported_value id ->
      Printf.sprintf "discarding %S (unsupported value)"
        (Ident.name id)
  | Discard_unsupported_type id ->
      Printf.sprintf "discarding %S (unsupported type)"
        (Ident.name id)
  | Discard_nested_module id ->
      Printf.sprintf "discarding %S (nested module)"
        (Ident.name id)
  | Discard_object_type id ->
      Printf.sprintf "discarding %S (object type should use class type definition)"
        (Ident.name id)

let verbose s =
  if !Args.verbose then
    print_endline s

let warning w =
  if not !Args.no_warnings then begin
    print_string "*** warning: ";
    print_endline (string_of_warning w)
  end
