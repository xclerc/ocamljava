(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java compiler is free software; you can redistribute it and/or modify
 * it under the terms of the Q Public License as published by
 * Trolltech (with a change to choice of law).
 *
 * OCaml-Java compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Q Public License for more details.
 *
 * You should have received a copy of the Q Public License
 * along with this program.  If not, see
 * <http://opensource.org/licenses/QPL-1.0>.
 *)

val compile_class :
  name:BaristaLibrary.Name.for_class ->
  parent:BaristaLibrary.Name.for_class ->
  fields:BaristaLibrary.Field.t list ->
  methods:BaristaLibrary.Method.t list ->
  attributes:BaristaLibrary.Attribute.for_class list ->
  BaristaLibrary.Bytes.t
(** Compiles to class file format the class definition whose elements are
    passed. *)

val make_startup_class : (Cmj_format.unit_infos * string * Digest.t) list -> string -> string * BaristaLibrary.Bytes.t
(** [make_startup_class units_to_link execname] compiles the startup
    class, and returns its name and class file contents. [units_to_link]
    contains the list of module to link, and [execname] is the name of
    the executable file to be created. *)

