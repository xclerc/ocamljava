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

(** Definition of types: names and conversion functions. *)

type conversion_function = JavaAST.expression -> JavaAST.expression
(** The type of conversion functions, mapping code fragments. *)

type t = {
    ocaml_type : Path.t option; (** OCaml type name. *)
    java_type : JavaAST.type_; (** Java type name. *)
    java_of_ocaml : conversion_function; (** Conversion from OCaml to Java. *)
    ocaml_of_java : conversion_function; (** Conversion from Java to OCaml. *)
  }
(** The type of type definitions. *)

val make_simple : Ident.t -> Path.t * t
(** [make_simple id] returns a [(path, info)] couple for the passed OCaml
    name [id], where [path] is just [id] unqualified and [info] contains
    the default conversion functions. *)

val predefined_types : bool -> t list
(** Returns the list of predefined types, the passed boolean indicating
    whether types are boxed. *)
