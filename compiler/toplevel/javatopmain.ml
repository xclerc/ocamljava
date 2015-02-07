(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (toplevel/opttopmain.ml in the OCaml source
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

open Clflags
open Jclflags

let usage = "Usage: ocamlj <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref ["javalib.cmja"]

let prepare ppf =
  Javatoploop.set_paths ();
  try
    let res =
      List.for_all (Javatopdirs.load_file ppf) (List.rev !preload_objects)
    in
    !Javatoploop.toplevel_startup_hook ();
    res
  with x ->
    try Javaerrors.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmjs"
    || Filename.check_suffix name ".cmj"
    || Filename.check_suffix name ".cmja"
  then preload_objects := name :: !preload_objects
  else
    begin
      let newargs = Array.sub Sys.argv !Arg.current
                              (Array.length Sys.argv - !Arg.current)
      in
      if prepare ppf && Javatoploop.run_script ppf name newargs
      then exit 0
      else exit 2
    end

let print_version () =
  Printf.printf "The OCaml toplevel, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
  exit 0;
;;

module Options = Java_args.Make_top_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _absname = set Location.absname
  let _compact = clear optimize_for_speed
  let _I dir =
    let dir = Misc.expand_directory Config.standard_library dir in
    include_dirs := dir :: !include_dirs
  let _init s = init_file := Some s
  let _inline n = inline_threshold := n * 8
  let _labels = clear classic
  let _no_app_funct = clear applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noprompt = set noprompt
  let _nopromptcont = set nopromptcont
  let _nostdlib = set no_std_include
  let _ppx s = Compenv.first_ppx := s :: !Compenv.first_ppx
  let _principal = set principal
  let _rectypes = set recursive_types
  let _short_paths = clear real_paths
  let _strict_sequence = set strict_sequence
  let _S = set keep_asm_file
  let _stdin () = file_argument ""
  let _unsafe = set fast
  let _version () = print_version ()
  let _vnum () = print_version_num ()
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings

  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _djlambda = set dump_jlambda
  let _dminstr = set dump_minstr
  let _dbytecode = set dump_bytecode
  let _doptbytecode = set dump_optbytecode
  let _dprimitives = set dump_primitives

  let anonymous = file_argument
end);;

let main () =
  let ppf = Format.err_formatter in
  Compenv.(readenv ppf Before_args);
  java_extensions := true;
  Arg.parse Options.list file_argument usage;
  Compenv.(readenv ppf Before_link);
  if not (prepare ppf) then exit 2;
  Javatoploop.loop Format.std_formatter
