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

val compile : Jclflags.servlet_kind -> string -> string -> Types.signature -> BaristaLibrary.Bytes.t
(** [compile kind mod_kind base_class_name signature] compiles a class
    implementing a servlet for kind [kind]/[mod_kind], named [base_class_name],
    whose signature is [signature]. Returns the contents of the compiled
    class file. *)
