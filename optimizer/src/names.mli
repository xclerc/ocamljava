(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java optimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java optimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** This module centralizes the various constant names used throughout
    the program. *)


(* {6 Class names} *)

val abstract_native_runner : BaristaLibrary.Name.for_class

val class_ : BaristaLibrary.Name.for_class

val constants_class : BaristaLibrary.Name.for_class

val current_context : BaristaLibrary.Name.for_class

val debug : BaristaLibrary.Name.for_class

val entry_point : BaristaLibrary.Name.for_class

val fail_exception : BaristaLibrary.Name.for_class

val global_uses : BaristaLibrary.Name.for_class

val no_signal_support : BaristaLibrary.Name.for_class

val object_ : BaristaLibrary.Name.for_class

val ocamljava_module : BaristaLibrary.Name.for_class

val signal_support : BaristaLibrary.Name.for_class

val string : BaristaLibrary.Name.for_class

val thread_local : BaristaLibrary.Name.for_class

val thread_local_factory : BaristaLibrary.Name.for_class

val value : BaristaLibrary.Name.for_class


(* {6 Method names} *)

val begin_ : BaristaLibrary.Name.for_method

val check_signals : BaristaLibrary.Name.for_method

val constants_storage : BaristaLibrary.Name.for_method

val create_constants : BaristaLibrary.Name.for_method

val cstr_name : BaristaLibrary.Name.for_method

val end_ : BaristaLibrary.Name.for_method

val enter_blocking_section : BaristaLibrary.Name.for_method

val entry : BaristaLibrary.Name.for_method

val fill_in_stack_trace : BaristaLibrary.Name.for_method

val get : BaristaLibrary.Name.for_method

val global_storage : BaristaLibrary.Name.for_method

val incr_globals_inited : BaristaLibrary.Name.for_method

val init_global_begin : BaristaLibrary.Name.for_method

val init_global_end : BaristaLibrary.Name.for_method

val leave_blocking_section : BaristaLibrary.Name.for_method

val main_scripting : BaristaLibrary.Name.for_method

val main_with_return : BaristaLibrary.Name.for_method

val module_main : BaristaLibrary.Name.for_method

val set_constant : BaristaLibrary.Name.for_method

val set_global : BaristaLibrary.Name.for_method

val shared_constants_begin : BaristaLibrary.Name.for_method

val shared_constants_end : BaristaLibrary.Name.for_method


(* {6 Field names} *)

val constants : BaristaLibrary.Name.for_field

val globals : BaristaLibrary.Name.for_field

val result : BaristaLibrary.Name.for_field

val unit : BaristaLibrary.Name.for_field


(* {6 Attribute names) *)

val linked_classes : BaristaLibrary.UTF8.t

val standalone : BaristaLibrary.UTF8.t
