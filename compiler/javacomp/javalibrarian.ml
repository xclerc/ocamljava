(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (asmcomp/asmlibrarian.ml in the OCaml source
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

type error =
    File_not_found of string
  | Archiver_error of string * string
  | File_compiled_with_a_different_int_size of string

exception Error of error

let read_info name =
  let filename =
    try
      Misc.find_in_path !Config.load_path name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let (info, crc) = Jcompilenv.read_unit_info filename in
  if info.Cmj_format.ui_ints_are_63_bit_long <> Jconfig.ints_are_63_bit_long
  then raise(Error(File_compiled_with_a_different_int_size filename));
  info.Cmj_format.ui_force_link <- !Clflags.link_everything;
  (* There is no need to keep the approximation in the .cmka file,
     since the compiler will go looking directly for .cmj files.
     The linker, which is the only one that reads .cmja files, does not
     need the approximation. *)
  info.Cmj_format.ui_approx <- Jlambda.Value_unknown None;
  (Filename.chop_suffix filename Jconfig.ext_compiled ^ Jconfig.ext_obj), (info, crc)

let create_archive file_list lib_name =
  let archive_name = Misc.chop_extension_if_any lib_name ^ Jconfig.ext_lib in
  let outchan = open_out_bin lib_name in
  try
    output_string outchan Jconfig.cmja_magic_number;
    let (objfile_list, descr_list) =
      List.split (List.map read_info file_list) in
    List.iter2
      (fun file_name (unit, crc) ->
        Javalink.check_consistency file_name unit crc)
      file_list descr_list;
    let infos =
      { Cmj_format.lib_units = descr_list;
        lib_ccobjs = !Clflags.ccobjs;
        lib_ints_are_63_bit_long = Jconfig.ints_are_63_bit_long; } in
    output_value outchan infos;
    close_out outchan;
    let ok = Archiveutils.copy_entries_list objfile_list archive_name in
    match ok with
    | Some msg -> raise(Error(Archiver_error (archive_name, msg)))
    | None -> ()
  with x ->
    close_out outchan;
    Misc.remove_file lib_name;
    Misc.remove_file archive_name;
    raise x

let report_error ppf = function
  | File_not_found name ->
      Format.fprintf ppf "Cannot find file %s" name
  | Archiver_error (name, msg) ->
      Format.fprintf ppf "Error while creating the library %s:@ %s" name msg
  | File_compiled_with_a_different_int_size name ->
      Format.fprintf ppf "File %s@ has been compiled with a different int size" name
