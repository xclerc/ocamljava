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

type t = java'lang'Runtime java_instance

let get_runtime () =
  Java.call "Runtime.getRuntime()" ()

let available_processors rt =
  Java.call "Runtime.availableProcessors()" rt

let free_memory rt =
  Java.call "Runtime.freeMemory()" rt

let max_memory rt =
  Java.call "Runtime.maxMemory()" rt

let total_memory rt =
  Java.call "Runtime.totalMemory()" rt
