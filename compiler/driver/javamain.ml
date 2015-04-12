(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (driver/optmain.ml in the OCaml source
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


let process_interface_file ppf name =
  Javacompile.interface ppf name (Compenv.output_prefix name)

let process_implementation_file ppf name =
  let opref = Compenv.output_prefix name in
  Javacompile.implementation ppf name opref;
  Clflags.objfiles := (opref ^ Jconfig.ext_compiled) :: !Clflags.objfiles

let cmja_present = ref false

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then
    process_implementation_file ppf name
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = Compenv.output_prefix name in
    Javacompile.interface ppf name opref;
    if !Clflags.make_package then Clflags.(objfiles := (opref ^ ".cmi") :: !objfiles)
  end
  else if Filename.check_suffix name Jconfig.ext_compiled then
    Clflags.(objfiles := name :: !objfiles)
  else if Filename.check_suffix name Jconfig.ext_library then begin
    cmja_present := true;
    Clflags.(objfiles := name :: !objfiles)
  end else if Filename.check_suffix name ".cmi" && !Clflags.make_package then
    Clflags.(objfiles := name :: !objfiles)
  else if Filename.check_suffix name Jconfig.ext_obj
       || Filename.check_suffix name Jconfig.ext_lib then
    Clflags.(ccobjs := name :: !ccobjs)
  else if Filename.check_suffix name ".java" then begin
    Javacompile.java_file name;
    Clflags.ccobjs :=
      (Filename.chop_suffix (Filename.basename name) ".java" ^ Jconfig.ext_class)
      :: !Clflags.ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let usage = "Usage: ocamljava <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  Compenv.(readenv ppf Before_compile);
  process_file ppf filename

let impl filename =
  Compenv.(readenv ppf Before_compile);
  process_implementation_file ppf filename

let intf filename =
  Compenv.(readenv ppf Before_compile);
  process_interface_file ppf filename

let show_config () =
  Jconfig.print_config stdout;
  exit 0

let print_version_and_library () =
  print_string "The OCaml Java-bytecode compiler, version ";
  print_string Config.version; print_newline();
  print_string "OCaml-Java version: ";
  print_string Jconfig.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

module Options = Java_args.Make_comp_options (struct
  let set r () = r := true
  let clear r () = r := false
  let set_applet_kind r k =
    let k = match k with
    | "graphics" -> Jclflags.Graphics
    | "awt"      -> Jclflags.Awt
    | "swing"    -> Jclflags.Swing
    | _ -> assert false in
    r := Some k
  let set_servlet_kind r k =
    let k = match k with
    | "generic"                     -> Jclflags.Generic
    | "http"                        -> Jclflags.Http
    | "context-listener"            -> Jclflags.Context_listener
    | "context-attribute-listener"  -> Jclflags.Context_attribute_listener
    | "session-listener"            -> Jclflags.Session_listener
    | "session-activation-listener" -> Jclflags.Session_activation_listener
    | "session-attribute-listener"  -> Jclflags.Session_attribute_listener
    | "session-binding-listener"    -> Jclflags.Session_binding_listener
    | "session-id-listener"         -> Jclflags.Session_id_listener
    | _                             -> assert false in
    r := Some k

  open Clflags
  open Jclflags

  let _a = set make_archive
  let _absname = set Location.absname
  let _additional_class s = additional_classes := s :: !additional_classes
  let _additional_file s = additional_files := s :: !additional_files
  let _additional_jar s = additional_jars := s :: !additional_jars
  let _additional_jar_ref s = additional_jar_refs := s :: !additional_jar_refs
  let _applet = set_applet_kind applet
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _classpath s = classpath_reset := true; classpath := [s]
  let _compact = clear optimize_for_speed
  let _config () = show_config ()
  let _cp s = classpath := !classpath @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I dir = include_dirs := dir :: !include_dirs
  let _impl = impl
  let _inline n = inline_threshold := n * 8
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _java_extensions = set java_extensions
  let _java_generics = set java_generics
  let _java_internal_types = set java_internal_types
  let _java_package s =
    try
      let open BaristaLibrary in
      ignore (Name.make_for_package_from_external (UTF8.of_string s));
      java_package := s
    with _ ->
      raise(Arg.Bad("invalid package name " ^ s))
  let _javac s = c_compiler := Some s
  let _jopt s = all_ccopts := s :: !all_ccopts
  let _labels = clear classic
  let _linkall = set link_everything
  let _no_app_funct = clear applicative_functors
  let _noassert = set noassert
  let _nobuiltin = set nobuiltin
  let _nolabels = set classic
  let _nomerge = set nomerge
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _opt_floats = set opt_floats
  let _opt_unroll_loops = set opt_unroll_loops
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = Compenv.(first_ppx := s :: !first_ppx)
  let _principal = set principal
  let _provider s = providers := s :: !providers
  let _rectypes = set recursive_types
  let _runtime_parameter s = runtime_parameters := s :: !runtime_parameters
  let _scripting = set scripting
  let _servlet = set_servlet_kind servlet
  let _shared () = shared := true; dlcode := true
  let _shared_libraries = clear standalone
  let _short_paths = clear real_paths
  let _signals = set signals
  let _standalone = set standalone
  let _strict_sequence = set strict_sequence
  let _thread = set use_threads
  let _unsafe = set fast
  let _v () = print_version_and_library ()
  let _version () = Compenv.print_version_string ()
  let _vnum () = Compenv.print_version_string ()
  let _verbose = set verbose
  let _w s = Warnings.parse_options false s
  let _war s = war := Some s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings
  let _where () = Compenv.print_standard_library ()

  let _nopervasives = set nopervasives
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

  let anonymous = anonymous
end);;

let default_output = function
  | Some s -> s
  | None ->
      match !Jclflags.war with
      | Some _ -> (Filename.chop_suffix Jconfig.default_executable_name ".jar") ^ ".war"
      | None   -> Jconfig.default_executable_name

let main () =
  Clflags.native_code := true;
  try
    Compenv.(readenv ppf Before_args);
    Arg.parse Options.list anonymous usage;
    Compenv.(readenv ppf Before_link);
    if (!Jclflags.applet <> None) && (!Jclflags.war <> None)
    then Compenv.fatal "Please specify at most one of -applet, -war";
    if List.length (List.filter (fun x -> !x)
                      [Clflags.make_package; Clflags.make_archive;
                       Clflags.shared; Clflags.compile_only]) > 1
        then Compenv.fatal "Please specify at most one of -pack, -a, -shared, -c";
    if !Clflags.make_archive then begin
      if !cmja_present then
        Compenv.fatal "Option -a cannot be used with .cmja input files.";
      Compmisc.init_path true;
      let target = Compenv.extract_output !Clflags.output_name in
      Javalibrarian.create_archive (Compenv.get_objfiles ()) target;
      Warnings.check_fatal ()
    end
    else if !Clflags.make_package then begin
      Compmisc.init_path true;
      let target = Compenv.extract_output !Clflags.output_name in
      Javapackager.package_files ppf (Compenv.get_objfiles ()) target;
      Warnings.check_fatal ()
    end
    else if !Clflags.shared then begin
      Compmisc.init_path true;
      let target = Compenv.extract_output !Clflags.output_name in
      Javalink.link_shared ppf (Compenv.get_objfiles ()) target;
      Warnings.check_fatal ()
    end
    else if not !Clflags.compile_only && !Clflags.objfiles <> [] then begin
      let target = default_output !Clflags.output_name in
      Compmisc.init_path true;
      Javalink.link ppf (Compenv.get_objfiles ()) target;
      Warnings.check_fatal ()
    end;
    exit 0
  with x ->
    Javaerrors.report_error ppf x;
    exit 2

let _ = main ()
