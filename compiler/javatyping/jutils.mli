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

(** Basic utility elements related to Java and typing. *)


val ocamldoc_mode : bool ref
(** Whether typing code is called from ocamldoc (in this case,
    [Path]/[Predef] cannot be used to know whether a given constructor is
    an ocamljava-specific one). *)

val get_class_loader : unit -> BaristaLibrary.ClassLoader.t
(** Returns the class loader used by the compiler, initializing it
    if necessary. *)

val get_unifier : unit -> BaristaLibrary.StackState.instance BaristaLibrary.StackState.unifier
(** Returns a unifier usinsg closest-common-parent policy, based on the
    class loader returned by [get_class_loader]. *)

val is_subtype : BaristaLibrary.Name.for_class -> BaristaLibrary.Name.for_class -> bool
(** [is_subtype x y] returns [true] iff [x] is a subtype of [y]. *)
