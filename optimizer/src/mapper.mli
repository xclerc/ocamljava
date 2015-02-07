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

(** Mappers for various structures. *)


class for_compiled_module : Misc.remove_index_function -> BaristaLibrary.ClassTraversal.class_definition_mapper
(** Mapper for OCaml modules compiled into Java classes by the OCaml-Java
    compiler. *)

class for_entry_point : BaristaLibrary.ClassTraversal.class_definition_mapper
(** Mapper for program entry points created at link time by the
    OCaml-Java compiler. *)

class for_runtime : bool -> BaristaLibrary.ClassTraversal.class_definition_mapper
(** Mapper for classes from the runtime support of OCaml-Java. *)

class for_archive : BaristaLibrary.ArchiveFile.t -> Misc.remove_index_function -> BaristaLibrary.ArchiveTraversal.archive_mapper
(**  Mapper for whole archives containing standalone OCaml programs. *)
