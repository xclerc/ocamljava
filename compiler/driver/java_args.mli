(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (driver/main_args.mli in the OCaml source
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

module type Comp_options = sig
  val _a : unit -> unit
  val _absname : unit -> unit
  val _additional_class : string -> unit
  val _additional_file : string -> unit
  val _additional_jar : string -> unit
  val _additional_jar_ref : string -> unit
  val _applet : string -> unit
  val _annot : unit -> unit
  val _binannot : unit -> unit
  val _c : unit -> unit
  val _classpath : string -> unit
  val _compact : unit -> unit
  val _config : unit -> unit
  val _cp : string -> unit
  val _for_pack : string -> unit
  val _g : unit -> unit
  val _i : unit -> unit
  val _I : string -> unit
  val _impl : string -> unit
  val _inline : int -> unit
  val _intf : string -> unit
  val _intf_suffix : string -> unit
  val _java_extensions : unit -> unit
  val _java_generics : unit -> unit
  val _java_internal_types : unit -> unit
  val _java_package : string -> unit
  val _javac : string -> unit
  val _javafx_application : unit -> unit
  val _jopt : string -> unit
  val _labels : unit -> unit
  val _linkall : unit -> unit
  val _no_app_funct : unit -> unit
  val _noassert : unit -> unit
  val _nobuiltin : unit -> unit
  val _nolabels : unit -> unit
  val _nomerge : unit -> unit
  val _nostdlib : unit -> unit
  val _o : string -> unit
  val _opt_floats : unit -> unit
  val _opt_unroll_loops : unit -> unit
  val _pack : unit -> unit
  val _pp : string -> unit
  val _ppx : string -> unit
  val _principal : unit -> unit
  val _provider : string -> unit
  val _rectypes : unit -> unit
  val _runtime_parameter : string -> unit
  val _scripting : unit -> unit
  val _servlet : string -> unit
  val _shared : unit -> unit
  val _shared_libraries : unit -> unit
  val _short_paths : unit -> unit
  val _signals : unit -> unit
  val _standalone : unit -> unit
  val _strict_sequence : unit -> unit
  val _thread : unit -> unit
  val _unsafe : unit -> unit
  val _v : unit -> unit
  val _verbose : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _war : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit
  val _where : unit -> unit

  val _nopervasives : unit -> unit
  val _dsource : unit -> unit
  val _dparsetree : unit -> unit
  val _dtypedtree : unit -> unit
  val _drawlambda : unit -> unit
  val _dlambda : unit -> unit
  val _djlambda : unit -> unit
  val _dminstr : unit -> unit
  val _dbytecode : unit -> unit
  val _doptbytecode : unit -> unit
  val _dprimitives : unit -> unit

  val anonymous : string -> unit
end;;

module type Top_options = sig
  val _absname : unit -> unit
  val _compact : unit -> unit
  val _I : string -> unit
  val _init : string -> unit
  val _inline : int -> unit
  val _labels : unit -> unit
  val _no_app_funct : unit -> unit
  val _noassert : unit -> unit
  val _nolabels : unit -> unit
  val _noprompt : unit -> unit
  val _nopromptcont : unit -> unit
  val _nostdlib : unit -> unit
  val _ppx : string -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _short_paths : unit -> unit
  val _stdin : unit -> unit
  val _strict_sequence : unit -> unit
  val _unsafe : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit

  val _dsource : unit -> unit
  val _dparsetree : unit -> unit
  val _dtypedtree : unit -> unit
  val _drawlambda : unit -> unit
  val _dlambda : unit -> unit
  val _djlambda : unit -> unit
  val _dminstr : unit -> unit
  val _dbytecode : unit -> unit
  val _doptbytecode : unit -> unit
  val _dprimitives : unit -> unit

  val anonymous : string -> unit
end;;

module type Arg_list = sig
    val list : (string * Arg.spec * string) list
end;;

module Make_comp_options (F : Comp_options) : Arg_list;;
module Make_top_options (F : Top_options) : Arg_list;;
