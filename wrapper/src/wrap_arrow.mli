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


val get_type_parameters : Types.type_expr list -> Types.type_expr -> TypeParametersTable.t

val generics_of_type_parameters : TypeParametersTable.t -> string list

val return_type_of_type_expr : ?reverse:bool -> TypeParametersTable.t -> Types.type_expr -> (JavaAST.type_ * TypeInfo.conversion_function * bool) option

val make_body : (JavaAST.type_ * (JavaAST.expression -> JavaAST.expression) * bool) option -> TypeParametersTable.t -> JavaAST.expression -> JavaAST.statement list

val make_try_catch : ?catch_all:bool -> JavaAST.block -> JavaAST.block

val throws_list : string list

val wrap_closure : string -> Ident.t -> Types.type_expr -> Jlambda.value_approximation option -> int -> JavaAST.method_ option
(** Returns the method wrapping the closure whose module name, name, type, approximation and global index are passed. *)

val wrap : string -> Ident.t -> Types.type_expr -> Jlambda.value_approximation option -> int -> JavaAST.method_
(** Returns the method wrapping the function whose module name, name, type, approximation and global index are passed. *)
