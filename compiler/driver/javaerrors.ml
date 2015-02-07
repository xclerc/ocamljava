(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (driver/opterrors.ml in the OCaml source
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

open Format

(* Keep synchronized with "ocamldoc/odoc_analyse.ml" *)

let report_error ppf exn =
  let report ppf = function
  (* errors common to ocamlc/ocamlopt/ocamljava *)
  | Lexer.Error(err, l) ->
      Location.print_error ppf l;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Pparse.Error err ->
      Pparse.report_error ppf err
  | Env.Error err ->
      Location.print_error_cur_file ppf;
      Env.report_error ppf err
  | Cmi_format.Error err ->
      Location.print_error_cur_file ppf;
      Cmi_format.report_error ppf err
  | Ctype.Tags(l, l') ->
      Location.print_error_cur_file ppf;
      fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value.@ Change one of them." l l'
  | Typecore.Error(loc, env, err) ->
      Location.print_error ppf loc; Typecore.report_error env ppf err
  | Typetexp.Error(loc, env, err) ->
      Location.print_error ppf loc; Typetexp.report_error env ppf err
  | Typedecl.Error(loc, err) ->
      Location.print_error ppf loc; Typedecl.report_error ppf err
  | Typeclass.Error(loc, env, err) ->
      Location.print_error ppf loc; Typeclass.report_error env ppf err
  | Includemod.Error err ->
      Location.print_error_cur_file ppf;
      Includemod.report_error ppf err
  | Typemod.Error(loc, env, err) ->
      Location.print_error ppf loc; Typemod.report_error env ppf err
  | Translcore.Error(loc, err) ->
      Location.print_error ppf loc; Translcore.report_error ppf err
  | Translclass.Error(loc, err) ->
      Location.print_error ppf loc; Translclass.report_error ppf err
  | Translmod.Error(loc, err) ->
      Location.print_error ppf loc; Translmod.report_error ppf err
  (* errors specific to ocamljava *)
  | Bytecodegen_misc.Error code ->
      Location.print_error_cur_file ppf;
      Bytecodegen_misc.report_error ppf code
  | Javalibrarian.Error code ->
      Location.print_error_cur_file ppf;
      Javalibrarian.report_error ppf code
  | Javalink.Error code ->
      Location.print_error_cur_file ppf;
      Javalink.report_error ppf code
  | Javagen.Error code ->
      Location.print_error_cur_file ppf;
      Javagen.report_error ppf code
  | Javapackager.Error code ->
      Location.print_error_cur_file ppf;
      Javapackager.report_error ppf code
  | Jcompilenv.Error code ->
      Location.print_error_cur_file ppf;
      Jcompilenv.report_error ppf code
  | Jclosure.Error code ->
      Location.print_error_cur_file ppf;
      Jclosure.report_error ppf code
  | Macrogen.Error code ->
      Location.print_error_cur_file ppf;
      Macrogen.report_error ppf code
  | Runtimeprimitives.Error code ->
      Location.print_error_cur_file ppf;
      Runtimeprimitives.report_error ppf code
  (* system errors and warnings *)
  | Sys_error msg ->
      Location.print_error_cur_file ppf;
      fprintf ppf "I/O error: %s" msg
  | Warnings.Errors n ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Some fatal warnings were triggered (%d occurrences)" n
  (* errors from the Barista library *)
  | x when BaristaLibrary.Predefined.is_barista_exception x ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Java error:@ %s"
        (BaristaLibrary.Predefined.string_of_exception x)
  (* unknown exception *)
  | x -> fprintf ppf "@]"; raise x in
  (* actual printing *)
  fprintf ppf "@[%a@]@." report exn
