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

(** Uses of global elements (read/write). *)

val reset_global_uses : unit -> unit
(** Resets the sets of global uses (for both reads and writes). *)

val record_global_uses : Jlambda.jlambda -> unit
(** Record all the global uses occurring in the passed lambda. *)

val compile_global_uses : unit -> BaristaLibrary.Annotation.t list
(** Compiles all recorded global uses into a list of
    {i org.ocamljava.runtime.annotations.markers.GlobalUses}
    annotations, each holding the sets of read and written indices for
    a given global. *)
