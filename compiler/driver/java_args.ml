(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (driver/main_args.ml in the OCaml source
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

let mk_a f =
  "-a", Arg.Unit f, " Build a library"
;;

let mk_absname f =
  "-absname", Arg.Unit f, " Show absolute filenames in error messages"
;;

let mk_additional_class f =
  "-additional-class", Arg.String f, "<classname>  Add a class to the created jar file"
;;

let mk_additional_file f =
  "-additional-file", Arg.String f, "<file[:archivepath]>  Add a file to the created jar file"
;;

let mk_additional_jar f =
  "-additional-jar", Arg.String f, "<file>  Add a jar file to dependency list"
;;

let mk_additional_jar_ref f =
  "-additional-jar-ref", Arg.String f, "<file>  Add a jar file to dependency list"
;;

let mk_applet f =
  "-applet", Arg.Symbol (["graphics"; "awt"; "swing"], f), " Build an applet"
;;

let mk_annot f =
  "-annot", Arg.Unit f, " Save information in <filename>.annot"
;;

let mk_binannot f =
  "-bin-annot", Arg.Unit f, " Save typedtree in <filename>.cmt"
;;

let mk_c f =
  "-c", Arg.Unit f, " Compile only (do not link)"
;;

let mk_classpath f =
  "-classpath", Arg.String f, "<path>  Set classpath to <path>"
;;

let mk_compact f =
  "-compact", Arg.Unit f, " Optimize code size rather than speed"
;;

let mk_config f =
  "-config", Arg.Unit f, " Print configuration values and exit"
;;

let mk_cp f =
  "-cp", Arg.String f, "<path>  Add <path> to the classpath"
;;

let mk_dtypes f =
  "-dtypes", Arg.Unit f, " (deprecated) same as -annot"
;;

let mk_for_pack f =
  "-for-pack", Arg.String f,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamljava -pack -o <ident>.cmj"
;;

let mk_g f =
  "-g", Arg.Unit f, " Record debugging information"
;;

let mk_i f =
  "-i", Arg.Unit f, " Print inferred interface"
;;

let mk_I f =
  "-I", Arg.String f, "<dir>  Add <dir> to the list of include directories"
;;

let mk_impl f =
  "-impl", Arg.String f, "<file>  Compile <file> as a .ml file"
;;

let mk_init f =
  "-init", Arg.String f, "<file>  Load <file> instead of default init file"
;;

let mk_inline f =
  "-inline", Arg.Int f, "<n>  Set aggressiveness of inlining to <n>"
;;

let mk_intf f =
  "-intf", Arg.String f, "<file>  Compile <file> as a .mli file"
;;

let mk_intf_suffix f =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;

let mk_java_extensions f =
  "-java-extensions", Arg.Unit f, " Enable Java extensions"
;;

let mk_java_generics f =
  "-java-generics", Arg.Unit (fun _ -> f (); Misc.fatal_error "not available in this build"), " Enable Java generics"
;;

let mk_java_internal_types f =
  "-java-internal-types", Arg.Unit f, " Show Java internal types"
;;

let mk_java_package f =
  "-java-package", Arg.String f, "<pack>  Set the Java package for produced classes"
;;

let mk_javac f =
  "-javac", Arg.String f, "<comp>  Use <comp> as the Java compiler"
;;

let mk_javafx_application f =
  "-javafx-application", Arg.Unit f, " Build a JavaFX application"
;;

let mk_jopt f =
  "-jopt", Arg.String f, "<opt>  Pass option <opt> to the Java compiler"
;;

let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_linkall f =
  "-linkall", Arg.Unit f, " Link all modules, even unused ones"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let mk_noassert f =
  "-noassert", Arg.Unit f, " Do not compile assertion checks"
;;

let mk_nobuiltin f =
  "-nobuiltin", Arg.Unit f, " Don't use builtin primitive list"
;;

let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let mk_nomerge f =
  "-nomerge", Arg.Unit f, " Don't merge service descriptors"
;;

let mk_noprompt f =
  "-noprompt", Arg.Unit f, " Suppress all prompts"
;;

let mk_nopromptcont f =
  "-nopromptcont", Arg.Unit f,
  " Suppress prompts for continuation lines of multi-line inputs"
;;

let mk_nostdlib f =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let mk_o f =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;

let mk_opt_floats f =
  "-opt-floats", Arg.Unit f, " Optimize float operations"
;;

let mk_opt_unroll_loops f =
  "-opt-unroll-loops", Arg.Unit f, " Unroll loops"
;;

let mk_pack f =
  "-pack", Arg.Unit f, " Package the given .cmj files into one .cmj"
;;

let mk_pp f =
  "-pp", Arg.String f, "<command>  Pipe sources through preprocessor <command>"
;;

let mk_ppx f =
  "-ppx", Arg.String f,
  "<command>  Pipe abstract syntax trees through preprocessor <command>"
;;

let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_provider f =
  "-provider", Arg.String f, "<classname>  Register <classname> as a primitive provider"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let mk_runtime_parameter f =
  "-runtime-parameter", Arg.String f, "<name=value>  Add binding to the runtime parameters"
;;

let mk_scripting f =
  "-scripting", Arg.Unit f, " Compile for Java scripting (for internal use)"
;;

let mk_servlet f =
  "-servlet", Arg.Symbol (["generic";
                           "http";
                           "filter";
                           "context-listener";
                           "context-attribute-listener";
                           "session-listener";
                           "session-activation-listener";
                           "session-attribute-listener";
                           "session-binding-listener";
                           "session-id-listener"], f),
  " Compile as a servlet"
;;

let mk_shared f =
  "-shared", Arg.Unit f, " Produce a dynlinkable plugin"
;;

let mk_shared_libraries f =
  "-shared-libraries", Arg.Unit f, " Link in shared mode (dependencies recorded in manifest)"
;;

let mk_short_paths f =
  "-short-paths", Arg.Unit f, " Shorten paths in types"
;;

let mk_signals f =
  "-signals", Arg.Unit f, " Generate code checking signals"
;;

let mk_standalone f =
  "-standalone", Arg.Unit f, " Link in standalone mode (no dependency)"
;;

let mk_stdin f =
  "-stdin", Arg.Unit f, " Read script from standard input"
;;

let mk_strict_sequence f =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let mk_thread f =
  "-thread", Arg.Unit f,
  " Generate code that supports the system threads library"
;;

let mk_unsafe f =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;

let mk_v f =
  "-v", Arg.Unit f,
  " Print compiler version and location of standard library and exit"
;;

let mk_verbose f =
  "-verbose", Arg.Unit f, " Print calls to external commands"
;;

let mk_version f =
  "-version", Arg.Unit f, " Print version and exit"
;;

let mk_vnum f =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_w f =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Warnings.defaults_w
;;

let mk_war f =
  "-war", Arg.String f, "<path>  Link as war file (passed file is web.xml)"
;;

let mk_warn_error f =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Warnings.defaults_warn_error
;;

let mk_warn_help f =
  "-warn-help", Arg.Unit f, " Show description of warning numbers"
;;

let mk_where f =
  "-where", Arg.Unit f, " Print location of standard library and exit"
;;

let mk_nopervasives f =
  "-nopervasives", Arg.Unit f, " (undocumented)"
;;

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f, " (undocumented)"
;;

let mk_dtypedtree f =
  "-dtypedtree", Arg.Unit f, " (undocumented)"
;;

let mk_drawlambda f =
  "-drawlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dsource f =
  "-dsource", Arg.Unit f, " (undocumented)"
;;

let mk_dlambda f =
  "-dlambda", Arg.Unit f, " (undocumented)"
;;

let mk_djlambda f =
  "-djlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dminstr f =
  "-dminstr", Arg.Unit f, " (undocumented)"
;;

let mk_dbytecode f =
  "-dbytecode", Arg.Unit f, " (undocumented)"
;;

let mk_doptbytecode f =
  "-doptbytecode", Arg.Unit f, " (undocumented)"
;;

let mk_dprimitives f =
  "-dprimitives", Arg.Unit f, " (undocumented)"
;;

let mk__ f =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

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
  val _stdin: unit -> unit
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

module Make_comp_options (F : Comp_options) =
struct
  let list = [
    mk_a F._a;
    mk_absname F._absname;
    mk_additional_class F._additional_class;
    mk_additional_file F._additional_file;
    mk_additional_jar F._additional_jar;
    mk_additional_jar_ref F._additional_jar_ref;
    mk_applet F._applet;
    mk_annot F._annot;
    mk_binannot F._binannot;
    mk_c F._c;
    mk_classpath F._classpath;
    mk_compact F._compact;
    mk_config F._config;
    mk_cp F._cp;
    mk_dtypes F._annot;
    mk_for_pack F._for_pack;
    mk_g F._g;
    mk_i F._i;
    mk_I F._I;
    mk_impl F._impl;
    mk_inline F._inline;
    mk_intf F._intf;
    mk_intf_suffix F._intf_suffix;
    mk_java_extensions F._java_extensions;
    mk_java_generics F._java_generics;
    mk_java_internal_types F._java_internal_types;
    mk_java_package F._java_package;
    mk_javac F._javac;
    mk_javafx_application F._javafx_application;
    mk_jopt F._jopt;
    mk_labels F._labels;
    mk_linkall F._linkall;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_nobuiltin F._nobuiltin;
    mk_nolabels F._nolabels;
    mk_nomerge F._nomerge;
    mk_nostdlib F._nostdlib;
    mk_o F._o;
    mk_opt_floats F._opt_floats;
    mk_opt_unroll_loops F._opt_unroll_loops;
    mk_pack F._pack;
    mk_pp F._pp;
    mk_ppx F._ppx;
    mk_principal F._principal;
    mk_provider F._provider;
    mk_rectypes F._rectypes;
    mk_runtime_parameter F._runtime_parameter;
    mk_scripting F._scripting;
    mk_servlet F._servlet;
    mk_shared F._shared;
    mk_shared_libraries F._shared_libraries;
    mk_short_paths F._short_paths;
    mk_signals F._signals;
    mk_standalone F._standalone;
    mk_strict_sequence F._strict_sequence;
    mk_thread F._thread;
    mk_unsafe F._unsafe;
    mk_v F._v;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_verbose F._verbose;
    mk_w F._w;
    mk_war F._war;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk_where F._where;

    mk_nopervasives F._nopervasives;
    mk_dsource F._dsource;
    mk_dparsetree F._dparsetree;
    mk_dtypedtree F._dtypedtree;
    mk_drawlambda F._drawlambda;
    mk_dlambda F._dlambda;
    mk_djlambda F._djlambda;
    mk_dminstr F._dminstr;
    mk_dbytecode F._dbytecode;
    mk_doptbytecode F._doptbytecode;
    mk_dprimitives F._dprimitives;

    mk__ F.anonymous;
  ]
end;;

module Make_top_options (F : Top_options) = struct
  let list = [
    mk_absname F._absname;
    mk_compact F._compact;
    mk_I F._I;
    mk_init F._init;
    mk_inline F._inline;
    mk_labels F._labels;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_nolabels F._nolabels;
    mk_noprompt F._noprompt;
    mk_nopromptcont F._nopromptcont;
    mk_nostdlib F._nostdlib;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_stdin F._stdin;
    mk_strict_sequence F._strict_sequence;
    mk_unsafe F._unsafe;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;

    mk_dparsetree F._dparsetree;
    mk_drawlambda F._drawlambda;
    mk_dlambda F._dlambda;
    mk_djlambda F._djlambda;
    mk_dminstr F._dminstr;
    mk_dbytecode F._dbytecode;
    mk_doptbytecode F._doptbytecode;
    mk_dprimitives F._dprimitives;

    mk__ F.anonymous;
  ]
end;;
