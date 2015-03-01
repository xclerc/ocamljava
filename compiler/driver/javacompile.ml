(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (driver/optcompile.ml in the OCaml source
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


let initial_env () =
  let env = Compmisc.initial_env () in
  if !Jclflags.java_extensions && (not !Clflags.nopervasives) then
    try
      Env.open_pers_signature "JavaPervasives" env
    with Not_found ->
      prerr_endline "Warning: JavaPervasives is not opened.";
      env
  else
    env

(* Compile a .mli file *)

let interface ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  Compmisc.init_path true;
  let modulename =
    String.capitalize(Filename.basename(Misc.chop_extension_if_any sourcefile)) in
  Compenv.check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let initial_env = initial_env () in
  try
    let ast =
      Pparse.file ppf inputfile Parse.interface Config.ast_intf_magic_number in
    if !Clflags.dump_parsetree then Format.fprintf ppf "%a@." Printast.interface ast;
    if !Clflags.dump_source then Format.fprintf ppf "%a@." Pprintast.signature ast;
    let tsg = Typemod.transl_signature initial_env ast in
    if !Clflags.dump_typedtree then Format.fprintf ppf "%a@." Printtyped.interface tsg;
    let sg = tsg.Typedtree.sig_type in
    if !Clflags.print_types then
      Format.fprintf Format.std_formatter "%a@." Printtyp.signature
        (Typemod.simplify_signature sg);
    ignore (Includemod.signatures initial_env sg sg);
    Typecore.force_delayed_checks ();
    Warnings.check_fatal ();
    if not !Clflags.print_types then begin
      let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
      Typemod.save_signature modulename tsg outputprefix sourcefile initial_env sg
    end;
    Pparse.remove_preprocessed inputfile;
    Stypes.dump (Some (outputprefix ^ ".annot"))
  with e ->
    Pparse.remove_preprocessed inputfile;
    Stypes.dump (Some (outputprefix ^ ".annot"));
    raise e

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

let implementation ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  Compmisc.init_path true;
  let modulename =
    String.capitalize(Filename.basename(Misc.chop_extension_if_any sourcefile)) in
  Compenv.check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = initial_env () in
  Jcompilenv.reset ?packname:!Clflags.for_package modulename;
  let cmjfile = outputprefix ^ Jconfig.ext_compiled in
  let objfile = outputprefix ^ Jconfig.ext_obj in
  try
    if !Clflags.print_types then ignore begin
      Pparse.file ppf inputfile Parse.implementation Config.ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
                  Printtyped.implementation_with_coercion
    end else begin
      Pparse.file ppf inputfile Parse.implementation Config.ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
                  Printtyped.implementation_with_coercion
      ++ Translmod.transl_store_implementation modulename
      +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      +++ Simplif.simplify_lambda
      +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Javagen.compile_implementation outputprefix ppf;
      Jcompilenv.save_unit_info cmjfile
    end;
    Warnings.check_fatal ();
    Pparse.remove_preprocessed inputfile;
    Stypes.dump (Some (outputprefix ^ ".annot"))
  with x ->
    Misc.remove_file objfile;
    Misc.remove_file cmjfile;
    Pparse.remove_preprocessed inputfile;
    Stypes.dump (Some (outputprefix ^ ".annot"));
    raise x

let java_file name =
  Location.input_name := name;
  if Jcomp.compile_file name <> 0 then exit 2
