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

(** Utility functions. *)


type remove_index_function = BaristaLibrary.Name.for_class -> int -> bool
(** The type of functions that are used to determine whether a global
    should be removed; the parameters are:
    - the class name;
    - the global's index for the class. *)

val make_class_name_ext : string -> BaristaLibrary.Name.for_class
(** Creates a class name from a string (assuming external form). *)

val make_method_name : string -> BaristaLibrary.Name.for_method
(** Creates a method name from a string. *)

val make_field_name : string -> BaristaLibrary.Name.for_field
(** Creates a field name from a string. *)

val global_class_of_class : BaristaLibrary.Name.for_class -> BaristaLibrary.Name.for_class
(** Returns the name of the class holding globals for the class whose
    name is passed. *)

val is_runtime_class : BaristaLibrary.Name.for_class -> bool
(** Tests whether the class whose name is passed is part of the
    OCaml-Java runtime. *)

val is_shared_constant : BaristaLibrary.Name.for_field -> bool
(** Tests whether the field whose name is passed is a field holding a
    shared constant. *)
