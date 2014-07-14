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

(** Handling of command-line parameters. *)


val no_backtrace : bool ref
(** Whether to assume absence of backtrace. *)

val no_debug : bool ref
(** Whether to remove debug statements. *)

val no_dynlink : bool ref
(** Whether to assume absence of dynamic linking. *)

val no_runtime_lock : bool ref
(** Whether to remove support for runtime lock. *)

val no_signals : bool ref
(** Whether to remove support for signals. *)

val no_unused_global : bool ref
(** Whether to remove initialization of unused globals. *)

val one_context : bool ref
(** Whether to assume unique context. *)

val unsafe : bool ref
(** Whether to use unsafe containers. *)

val verbose : bool ref
(** Whether to enable verbose mode. *)

val war : bool ref
(** Whether optimized file is a war file. *)

val files : string list ref
(** Files provided on the command line. *)

val parse : unit -> unit
(** Parses the command line, and initializes the values exported by this
    module. *)
