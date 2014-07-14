(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (asmcomp/asmpackager.ml in the OCaml source
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
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error of string
  | File_not_found of string
  | File_compiled_with_a_different_int_size of string


exception Error of error

(* Read the unit information from a .cmj file. *)

type pack_member_kind = PM_intf | PM_impl of Cmj_format.unit_infos

type pack_member =
  { pm_file: string;
    pm_name: string;
    pm_kind: pack_member_kind }

let read_member_info pack_path file = (
  let name =
    String.capitalize(Filename.basename(Misc.chop_extensions file)) in
  let kind =
    if Filename.check_suffix file Jconfig.ext_compiled then begin
      let (info, crc) = Jcompilenv.read_unit_info file in
      if info.Cmj_format.ui_name <> name
      then raise(Error(Illegal_renaming(name, file, info.Cmj_format.ui_name)));
      if Cmj_format.(info.ui_symbol <>
         (Jcompilenv.current_unit_infos ()).ui_symbol ^ "__" ^ info.ui_name)
      then raise(Error(Wrong_for_pack (file, pack_path)));
      Javalink.check_consistency file info crc;
      Jcompilenv.cache_unit_info info;
      PM_impl info
    end else
      PM_intf in
  { pm_file = file; pm_name = name; pm_kind = kind }
)

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl infos ->
          if infos.Cmj_format.ui_ints_are_63_bit_long <> Jconfig.ints_are_63_bit_long
          then raise(Error(File_compiled_with_a_different_int_size mb.pm_file));
          List.iter
            (fun (unit, _) ->
              if List.mem unit forbidden
              then raise(Error(Forward_reference(mb.pm_file, unit))))
            infos.Cmj_format.ui_imports_cmj
      end;
      check (Misc.list_remove mb.pm_name forbidden) tl in
  check (List.map (fun mb -> mb.pm_name) members) members

(* Make the .jo file for the package *)

let make_package_object ppf members targetobj targetname coercion =
  let objtemp =
    Filename.temp_file (Jcompilenv.make_symbol (Some "")) Jconfig.ext_obj in
  let components =
    List.map
      (fun m ->
        match m.pm_kind with
        | PM_intf -> None
        | PM_impl _ -> Some(Ident.create_persistent m.pm_name))
      members in
  Javagen.compile_implementation
    (Misc.chop_extension_if_any objtemp) ppf
    (Translmod.transl_store_package
       components (Ident.create_persistent targetname) coercion);
  let objfiles =
    List.map
      (fun m -> Misc.chop_extension_if_any m.pm_file ^ Jconfig.ext_obj)
      (List.filter (fun m -> m.pm_kind <> PM_intf) members) in
  let ok =
    Archiveutils.copy_entries_list (objtemp :: objfiles) targetobj
  in
  Misc.remove_file objtemp;
  match ok with
  | Some msg -> raise(Error(Linking_error msg))
  | None -> ()

(* Make the .cmj file for the package *)

module StringSet = Set.Make (String)

let union l =
  List.fold_left
    (fun acc elem -> StringSet.add elem acc)
    StringSet.empty
    (List.concat l)
  |> StringSet.elements

let build_package_cmj members cmjfile =
  let unit_names =
    List.map (fun m -> m.pm_name) members in
  let filter lst =
    List.filter (fun (name, _crc) -> not (List.mem name unit_names)) lst in
  let units =
    List.fold_right
      (fun m accu ->
        match m.pm_kind with PM_intf -> accu | PM_impl info -> info :: accu)
      members [] in
  let ui = Jcompilenv.current_unit_infos () in
  let pkg_infos =
    let open Cmj_format in
    { ui_name = ui.ui_name;
      ui_javaname = ui.ui_javaname;
      ui_additional_classes = List.flatten
        (List.map
           (fun info ->
             info.ui_additional_classes @ [info.ui_javaname])
           units);
      ui_symbol = ui.ui_symbol;
      ui_defines =
          List.flatten (List.map (fun info -> info.ui_defines) units) @
          [ui.ui_symbol];
      ui_imports_cmi =
          (ui.ui_name, Env.crc_of_unit ui.ui_name) ::
          filter(Javalink.extract_crc_interfaces());
      ui_imports_cmj =
          filter(Javalink.extract_crc_implementations());
      ui_approx = ui.ui_approx;
      ui_force_link =
          List.exists (fun info -> info.ui_force_link) units;
      ui_ensure_init = union (List.map (fun info -> info.ui_ensure_init) units);
      ui_ints_are_63_bit_long = Jconfig.ints_are_63_bit_long;
    } in
  Jcompilenv.write_unit_info pkg_infos cmjfile

(* Make the .cmj and the .jo for the package *)

let package_object_files ppf files targetcmj
                         targetobj targetname coercion =
  let pack_path =
    match !Clflags.for_package with
    | None -> targetname
    | Some p -> p ^ "." ^ targetname in
  let members = Misc.map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object ppf members targetobj targetname coercion;
  build_package_cmj members targetcmj

(* The entry point *)

let package_files ppf files targetcmj =
  let files =
    List.map
      (fun f ->
        try Misc.find_in_path !Config.load_path f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = Misc.chop_extensions targetcmj in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = Misc.chop_extension_if_any targetcmj ^ Jconfig.ext_obj in
  let targetname = String.capitalize(Filename.basename prefix) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmj;
  Jcompilenv.reset ?packname:!Clflags.for_package targetname;
  try
    let coercion = Typemod.package_units files targetcmi targetname in
    package_object_files ppf files targetcmj targetobj targetname coercion
  with x ->
    Misc.remove_file targetcmj; Misc.remove_file targetobj;
    raise x

(* Error report *)

let report_error ppf = function
  | Illegal_renaming(name, file, id) ->
      Format.fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %s when %s was expected"
        Location.print_filename file name id
  | Forward_reference(file, ident) ->
      Format.fprintf ppf "Forward reference to %s in file %a" ident
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      Format.fprintf ppf "File %a@ was not compiled with the `-for-pack %s' option"
              Location.print_filename file path
  | File_not_found file ->
      Format.fprintf ppf "File %s not found" file
  | Linking_error msg ->
      Format.fprintf ppf "Error during partial linking:@ %s" msg
  | File_compiled_with_a_different_int_size file ->
      Format.fprintf ppf "File %a@ has been compiled with a different int size"
              Location.print_filename file
