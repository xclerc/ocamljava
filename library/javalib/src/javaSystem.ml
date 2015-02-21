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


(* Properties *)

let clear_property name =
  Java.call "System.clearProperty(String)" name

let get_property name =
  Java.call "System.getProperty(String)" name

let get_property_default name default =
  Java.call "System.getProperty(String,String)" name default

let set_property name value =
  Java.call "System.setProperty(String,String)" name value


(* Time *)

let current_time_millis () =
  Java.call "System.currentTimeMillis()" ()

let nano_time () =
  Java.call "System.nanoTime()" ()


(* Garbage collector *)

let gc () =
  Java.call "System.gc()" ()

let run_finalization () =
  Java.call "System.runFinalization()" ()


(* Environment *)

let get_env name =
  Java.call "System.getenv(String)" name
