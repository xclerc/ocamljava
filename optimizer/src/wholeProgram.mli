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

(** Whole-program information. *)


val shared_constants_class_name : BaristaLibrary.Name.for_class ref
(** The name of the class holding shared constants.

    Initialized by [iter_archive]. *)

val is_constants_class : BaristaLibrary.Name.for_class -> bool
(** Tests whether the passed class is a class holding constants.

    Initialized by [iter_archive]. *)

val main_class_of_constants_class : BaristaLibrary.Name.for_class -> BaristaLibrary.Name.for_class
(** Returns the class name for the passed constant-holding class.

    Initialized by [iter_archive]. *)

val additional_fields : BaristaLibrary.Name.for_class -> BaristaLibrary.Field.t list
(** Returns the additional fields for the passed class.

    Initialized by [iter_archive]. *)

val make_remove_indices_function : unit -> Misc.remove_index_function
(** Builds and returns a function indicating whether a given global
    should be discarded.

    Initialized by [iter_archive]. *)

val iter_archive : BaristaLibrary.Path.t -> unit
(** Initializes whole-program information, by iterating over the classes
    in the archive whose path is passed. *)

val add_shared_constant : BaristaLibrary.Name.for_field -> unit
(** Adds the passed field to the set of shared constants. *)

val compile_shared_constant_class : unit -> BaristaLibrary.UTF8.t * BaristaLibrary.Bytes.t
(** Compiles the class holding shared constants.

    Returns both the path inside the archive and the compiled class. *)
