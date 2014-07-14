(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Pretty-printing for Jlambda structure. *)


val loop_inlining_info : Format.formatter -> Jlambda.jlamdba_loop_inlining_info -> unit
(** [loop_inlining_info fmt lii] prints loop inlining info [lii] onto
    [fmt]. *)

val java_primitive : Format.formatter -> Jlambda.java_primitive -> unit
(** [java_primitive fmt jp] prints the primitive [jp] onto [fmt]. *)

val const : Format.formatter -> Jlambda.const -> unit
(** [const fmt cst] prints the constant [cst] onto [fmt]. *)

val jlambda : Format.formatter -> Jlambda.jlambda -> unit
(** [jlambda fmt j] prints the jlambda [j] onto [fmt]. *)
