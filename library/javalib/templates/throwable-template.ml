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


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"


(* Instance creation *)

type t = $(ocaml_short_name) java_instance

let make ?(cause = null) ?(message = JavaString.null) () =
  if JavaString.is_null message then
    Java.make "$(java_short_name)(Throwable)" cause
  else
    Java.make "$(java_short_name)(String,Throwable)" message cause


(* Throw *)

external throw : t -> 'a = "java throw"


(* Base methods *)

let get_cause th =
  Java.call "Throwable.getCause()" th

let get_message th =
  Java.call "Throwable.getMessage()" th
    
let get_stack_trace th =
  Java.call "Throwable.getStackTrace()" th

let print_stack_trace th =
  Java.call "Throwable.printStackTrace()" th


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
