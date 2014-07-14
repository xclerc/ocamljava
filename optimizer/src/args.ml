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


let no_backtrace = ref true

let no_debug = ref true

let no_dynlink = ref true

let no_runtime_lock = ref true

let no_signals = ref true

let no_unused_global = ref false

let one_context = ref true

let unsafe = ref false

let verbose = ref false

let war = ref false

let files = ref []

let add_file x =
  files := x :: !files

let switches = [
  "-no-backtrace",
  Arg.Bool (fun b -> no_backtrace := b),
  "<bool>  Whether to assume absence of backtrace (default: true)" ;

  "-no-debug",
  Arg.Bool (fun b -> no_debug := b),
  "<bool>  Whether to remove debug statements (default: true)" ;

  "-no-dynlink",
  Arg.Bool (fun b -> no_dynlink := b),
  "<bool>  Whether to assume absence of dynlink (default: true)" ;

  "-no-runtime-lock",
  Arg.Bool (fun b -> no_runtime_lock := b),
  "<bool>  Whether to remove support for runtime lock (default: true)" ;

  "-no-signals",
  Arg.Bool (fun b -> no_signals := b),
  "<bool>  Whether to remove support for signals (default: true)" ;

  "-no-unused-global",
  Arg.Bool (fun b -> no_unused_global := b),
  "<bool> Whether to remove initialization of unused globals (default: false)" ;

  "-one-context",
  Arg.Bool (fun b -> one_context := b),
  "<bool>  Whether to assume unique context (default: true)" ;

  "-unsafe",
  Arg.Bool (fun b -> unsafe := b),
  "<bool>  Whether to use unsafe containers (default: false)" ;

  "-verbose",
  Arg.Set verbose,
  " Enable verbose mode" ;

  "-war",
  Arg.Set war,
  " Optimized file is a war file" ;
]

let usage =
  Printf.sprintf "Usage: %s <options> in-file out-file\nOptions are:"
    (Filename.basename Sys.argv.(0))

let parse () =
  Arg.parse switches add_file usage
