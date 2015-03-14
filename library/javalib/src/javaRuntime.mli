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

(** Runtime information. *)


type t = java'lang'Runtime java_instance
(** The type of runtime instances. *)

val get_runtime : unit -> t
(** Returns the runtime for the current Java application; see
    {java java.lang.Runtime#getRuntime()}. *)

val available_processors : t -> java_int
(** Returns the number of (logical) available processors; see
    {java java.lang.Runtime#availableProcessors()}. *)

val free_memory : t -> java_long
(** Returns the amount of free memory in the JVM (in bytes); see
    {java java.lang.Runtime#freeMemory()}. *)

val max_memory : t -> java_long
(** Returns the maximum amount of memory to be used by the JVM
    (in bytes); see {java java.lang.Runtime#maxMemory()}. *)

val total_memory : t -> java_long
(** Returns the total amount of memory used by the JVM (in bytes); see
    {java java.lang.Runtime#totalMemory()}. *)
