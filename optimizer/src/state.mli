(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** This module centralizes various mutable variables used throughout the
    program. *)


val input_file : string ref
(** The name of the input file, should not change once set. *)

val current_class_name : BaristaLibrary.Name.for_class ref
(** The name of the currently processed class. *)

val get_class_loader : unit -> BaristaLibrary.ClassLoader.t
(** Returns the class loader lazily created at the first call. *)

val get_unifier : unit -> BaristaLibrary.StackState.instance BaristaLibrary.StackState.unifier
(** Returns the unifier based on the class loader returned by
    [get_class_loader]. *)
