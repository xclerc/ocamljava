(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


module type T = sig
  type e
  type 'a t
  val make : int32 -> e t
  val init : int32 -> (int32 -> e) -> e t
  val length : e t -> int32
  val get : e t -> int32 -> e
  val set : e t -> int32 -> e -> unit
  val append : e t -> e t -> e t
  val concat : e t list -> e t
  val sub : e t -> int32 -> int32 -> e t
  val copy : e t -> e t
  val fill : e t -> int32 -> int32 -> e -> unit
  val blit : e t -> int32 -> e t -> int32 -> int32 -> unit
  val to_list : e t -> e list
  val of_list : e list -> e t
  val iter : (e -> unit) -> e t -> unit
  val map : (e -> e) -> e t -> e t
  val iteri : (int32 -> e -> unit) -> e t -> unit
  val mapi : (int32 -> e -> e) -> e t -> e t
  val fold_left : ('a -> e -> 'a) -> 'a -> e t -> 'a
  val fold_right : (e -> 'a -> 'a) -> e t -> 'a -> 'a
  val of_ocaml : e array -> e t
  val to_ocaml : e t -> e array
  val to_object : e t -> java'lang'Object java_instance
  val of_object : java'lang'Object java_instance -> e t
  val equals : e t -> e t -> java_boolean
  val hash_code : e t -> java_int
  val to_string : e t -> JavaString.t
  val null : e t
  val is_null : e t -> bool
  val is_not_null : e t -> bool
  val wrap : e t -> e t option
  val unwrap : e t option -> e t
end
