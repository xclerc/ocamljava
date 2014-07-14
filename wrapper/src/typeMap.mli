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

(** This module maintains a map from OCaml types to type informations. *)


val add_local : Ident.t -> bool -> unit
(** [add_local id exp] adds a type definition for name [id], [exp]
    indicating whether the type should not be expanded. *)

val is_defined_and_doesnt_expand : Path.t -> bool
(** Tests whether the type whose path is passed is both defined and
    should not be expanded. *)

val promote : string -> unit
(** [promote modname] promotes the locally-defined types to be declared
    as part of the module with name [modname]. *)

val make_wrapper : TypeParametersTable.t -> JavaAST.type_ -> JavaAST.expression
(** [make_wrapper generics typ] returns the code fragment building the
    wrapper for type [typ], [generics] indicating how to access to
    generic types. *)

val find : ?generics:TypeParametersTable.t -> bool -> Types.type_expr -> TypeInfo.t
(** [find ~generics b te] returns the type information for type [te],
    [generics] indicating how to access to generic types and [b] whether
    the type is boxed.

    Raises [Wrap_common.Exception] if no type information is found. *)
