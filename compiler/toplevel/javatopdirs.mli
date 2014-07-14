(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (toplevel/opttopdirs.mli in the OCaml source
 * distribution) is Copyright (C) INRIA.
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

(* The toplevel directives. *)

open Format

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_remove_directory : string -> unit
val dir_cd : string -> unit
val dir_load : formatter -> string -> unit
val dir_use : formatter -> string -> unit
val dir_install_printer : formatter -> Longident.t -> unit
val dir_remove_printer : formatter -> Longident.t -> unit

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

val load_file : formatter -> string -> bool
