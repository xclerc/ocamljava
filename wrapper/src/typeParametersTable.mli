(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Mapping from parameter type names to location in instance. *)

type location =
  | Parameter of int list
      (** The type parameter is accessed by successive gets to passed indices. *)
  | Field of string
      (** The type parameter is acessed by getting the passed field. *)
  | Nowhere
      (** The type parameter cannot be accessed. *)
(** The type of parameter type locations. *)

type t
(** The type of maps from parameter type names to locations. *)

val empty : t
(** The empty map. *)

val make : (string * 'a) list -> t
(** Changes [[id_1, _; ...; id_n, _]] to a map associating [id_i] to
    [Field "w_" ^ id_i]. *)

val size : t -> int
(** Returns the number of elements in passed map. *)

val mem : string -> t -> bool
(** [mem k m] tests whether map [m] has a key [k]. *)

val find : string -> t -> location
(** [find k m] returns the value associated to key [k] in [m].

    Raises [Not_found] if no such value exists. *)

val add : string -> location -> t -> t
(** [add k v m] adds to map [m] a binding from key [k] to value [v]. *)

val iter : (string -> location -> unit) -> t -> unit
(** Iterator over maps. *)

val fold : (string -> location -> 'a -> 'a) -> t -> 'a -> 'a
(** Folder over maps. *)

val add_parameter : t ref -> string -> int list -> unit
(** [add_parameter m k l] adds a binding from key [k] to value
    [Parameter l] to map [m]. *)

val add_field : t ref -> string -> string -> unit
(** [add_field m k f] adds a binding from key [k] to value [Field f] to
    map [m]. *)

val add_nowhere : t ref -> string -> unit
(** [add_nowhere m k] adds a binding from key [k] to value [Nowhere] to
    map [m]. *)
