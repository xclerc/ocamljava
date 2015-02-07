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

(** Instruction lists stored in trees, to avoid concatenation
    operations. *)

type t = private
  | Node of int * (t list)
      (** Internal tree node, storing size and children. *)
  | Leaf of int * (BaristaLibrary.Instruction.t list)
      (** Tree leaf, storing size and instruction list. *)
  | Suspended of int * (BaristaLibrary.Instruction.t list lazy_t)
      (** Tree leaf, lazily evaluated. *)

val size : t -> int
(** Returns the size of the passed tree. *)

val node : t list -> t
(** [node l] builds an internal node with children [l]. *)

val leaf : ?ofs:int -> BaristaLibrary.Instruction.t list -> t
(** [leaf ~ofs:o l] builds a leaf with offset [o] and instructions [l].
    The offset (ignored if negative) is used only to compute the size of
    the list.

    Fails (with a fatal error) if no offset is passed and the instruction
    list contains a switch instruction. *)

val suspended : int -> BaristaLibrary.Instruction.t list lazy_t -> t
(** [suspended sz lz] build a leaf node that is lazily evaluated.
    Once evaluated the instruction list must have size [sz]. *)

val flatten : t -> BaristaLibrary.Instruction.t list
(** Converts the passed tree into a bare list, by visiting the tree and
    evaluatint suspended nodes. *)

val map : (BaristaLibrary.Instruction.t -> BaristaLibrary.Instruction.t) -> t -> t
(** [map f t] maps the tree [t] by applying [f] to leaf nodes.

    Suspended nodes are not evaluated.

    [f] must preserve the size of mapped instruction lists. *)
