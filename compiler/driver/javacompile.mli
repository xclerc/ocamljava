(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (driver/optcompile.mli in the OCaml source
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

(** Compile a .mli, .ml, or .java file *)

val interface: Format.formatter -> string -> string -> unit
(** [interface fmt src prefix] compiles the interface file whose path is
    [src] and produces files with prefix [prefix]. [fmt] is used for
    output (errors, warnings, etc.). *)

val implementation: Format.formatter -> string -> string -> unit
(** [implementation fmt src prefix] compiles the implementation file
    whose path is [src] and produces files with prefix [prefix]. [fmt] is
    used for output (errors, warnings, etc.). *)

val java_file: string -> unit
(** Compiles the file whose path is passed using a Java compiler. *)
