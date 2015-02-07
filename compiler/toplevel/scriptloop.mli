(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (toplevel/opttoploop.mli in the OCaml source
 * distribution) is Copyright (C) INRIA.
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

(** Implementation of scripting engine. *)


val set_paths : unit -> unit
(** Initializes load path. *)

val init : unit -> unit
(** Initializes the scripting engine, to make it ready to evaluate
    toplevel phrases. Registers the following callbacks:
    - ["ocamljava javax.script.eval"] of type [string -> 'a]
      that evaluates the passed phrase and returns the result of its
      evaluation, raising an exception if evaluation either fails or
      raises an exception;
    - ["ocamljava javax.script.directory"] of type [string -> unit]
      that adds the passed directory to the search path;
    - ["ocamljava javax.script.load"] of type [string -> unit]
      that loads the file whose path is passed. *)
