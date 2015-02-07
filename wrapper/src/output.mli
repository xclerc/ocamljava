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

(** Verbose/warning output. *)


type warning =
  | Discard_unit_value of Ident.t
  | Discard_object_value of Ident.t
  | Discard_polymorphic_variant_value of Ident.t
  | Discard_unsupported_value of Ident.t
  | Discard_unsupported_type of Ident.t
  | Discard_nested_module of Ident.t
  | Discard_object_type of Ident.t

val verbose : string -> unit
(** Prints the passed string, if enabled by command-line. *)

val warning : warning -> unit
(** Prints the passed warning, unless disabled by command-line. *)
