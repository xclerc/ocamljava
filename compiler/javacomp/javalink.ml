(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/asmlink.ml in the OCaml source
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

open BaristaLibrary
open Jcompilenv
open Javalink_startup
open Javalink_applet
open Javalink_servlet

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Linking_error of string
  | Multiple_definition of string * string * string
  | Missing_cmj of string * string
  | Unable_to_add_class of string
  | Unable_to_add_file of string
  | Duplicate_entry of string
  | Invalid_applet_signature of string
  | File_compiled_with_a_different_int_size of string
  | Invalid_runtime_parameter of string

exception Error of error

(* Consistency check between interfaces and implementations *)

let crc_interfaces = Consistbl.create ()
let crc_implementations = Consistbl.create ()
let extra_implementations = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)
let cmj_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crc) ->
        if name = unit.Cmj_format.ui_name
        then Consistbl.set crc_interfaces name crc file_name
        else Consistbl.check crc_interfaces name crc file_name)
      unit.Cmj_format.ui_imports_cmi
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crc) ->
        if crc <> cmj_not_found_crc then
          Consistbl.check crc_implementations name crc file_name
        else if List.mem name !cmj_required then
          raise(Error(Missing_cmj(file_name, name)))
        else
          extra_implementations := name :: !extra_implementations)
      unit.Cmj_format.ui_imports_cmj
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  begin try
    let source = List.assoc unit.Cmj_format.ui_name !implementations_defined in
    raise (Error(Multiple_definition(unit.Cmj_format.ui_name, file_name, source)))
  with Not_found -> ()
  end;
  Consistbl.set crc_implementations unit.Cmj_format.ui_name crc file_name;
  implementations_defined :=
    (unit.Cmj_format.ui_name, file_name) :: !implementations_defined;
  if unit.Cmj_format.ui_symbol <> unit.Cmj_format.ui_name then
    cmj_required := unit.Cmj_format.ui_name :: !cmj_required

let extract_crc_interfaces () =
  Consistbl.extract crc_interfaces
let extract_crc_implementations () =
  List.fold_left
    (fun ncl n ->
      if List.mem_assoc n ncl then ncl else (n, cmj_not_found_crc) :: ncl)
    (Consistbl.extract crc_implementations)
    !extra_implementations

(* First pass: determine which units are needed *)

let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, _crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg

type file =
  | Unit of string * Cmj_format.unit_infos * Digest.t
  | Library of string * Cmj_format.library_infos

let read_file obj_name =
  let file_name =
    try
      Misc.find_in_path !Config.load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name Jconfig.ext_compiled then begin
    (* This is a .cmj file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = Jcompilenv.read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name Jconfig.ext_library then begin
    let infos =
      try Jcompilenv.read_library_info file_name
      with Jcompilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file obj_name tolink = match read_file obj_name with
  | Unit (file_name,info,crc) ->
      (* This is a .cmj file. It must be linked in any case. *)
      remove_required info.Cmj_format.ui_name;
      List.iter (add_required file_name) info.Cmj_format.ui_imports_cmj;
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      List.fold_right
        (fun (info, crc) reqd ->
           if info.Cmj_format.ui_force_link
             || !Clflags.link_everything
             || is_required info.Cmj_format.ui_name
           then begin
             remove_required info.Cmj_format.ui_name;
             List.iter (add_required (Printf.sprintf "%s(%s)"
                                        file_name info.Cmj_format.ui_name))
               info.Cmj_format.ui_imports_cmj;
             (info, file_name, crc) :: reqd
           end else
             reqd)
         infos.Cmj_format.lib_units tolink

(* Second pass: generate the startup file and link it with everything else *)

let link_shared ppf objfiles output_name =
  ignore ppf;
  let plugin_header units =
    let conv (ui, crc) =
      Cmj_format.({ dynu_name = ui.ui_name;
                    dynu_crc = crc;
                    dynu_imports_cmi = ui.ui_imports_cmi;
                    dynu_imports_cmj = ui.ui_imports_cmj;
                    dynu_defines = ui.ui_defines }) in
    let header = Cmj_format.({ dynu_magic = Jconfig.cmjs_magic_number;
                               dynu_units = List.map conv units }) in
    Marshal.to_string header [] in
  let plugin_members units =
    let buff = Buffer.create 1024 in
    List.iter
      (fun (ui, _) ->
        Printf.sprintf "%s=%s\n" ui.Cmj_format.ui_name ui.Cmj_format.ui_javaname
        |> Buffer.add_string buff;
        if ui.Cmj_format.ui_additional_classes <> [] then begin
          String.concat "," ui.Cmj_format.ui_additional_classes
          |> Printf.sprintf "%s*=%s\n" ui.Cmj_format.ui_name
          |> Buffer.add_string buff
        end)
      units;
    Buffer.contents buff in
  let units_tolink = List.fold_right scan_file objfiles [] in
  List.iter
    (fun (info, file_name, crc) -> check_consistency file_name info crc)
    units_tolink;
  List.iter
    (fun (info, file_name, _crc) ->
      if info.Cmj_format.ui_ints_are_63_bit_long <> Jconfig.ints_are_63_bit_long
      then raise(Error(File_compiled_with_a_different_int_size file_name)))
    units_tolink;
  let units = List.map (fun (ui, _, crc) -> (ui, crc)) units_tolink in
  try
    let builder = Archiveutils.open_builder output_name in
    units
    |> plugin_header
    |> Bytes.make_of_string
    |> Archiveutils.add_entry builder "/PluginHeader";
    units
    |> plugin_members
    |> Bytes.make_of_string
    |> Archiveutils.add_entry builder "/PluginMembers";
    let cmj_files =
      List.filter
        (fun file -> Filename.check_suffix file Jconfig.ext_compiled)
        objfiles in
    List.iter
      (fun cmj_file ->
        let cmj_file_in_path =
          try
            Misc.find_in_path
              !Config.load_path
              (Filename.chop_suffix cmj_file Jconfig.ext_compiled ^ Jconfig.ext_obj)
          with Not_found -> raise(Error(File_not_found cmj_file)) in
        let cmj_archive =
          cmj_file_in_path
          |> Path.make_of_string
          |> ArchiveFile.make_of_path in
        ArchiveBuilder.add_entries_from_archive builder cmj_archive;
        ArchiveFile.close cmj_archive)
      cmj_files;
    Archiveutils.close builder
  with
  | (Error _) as e -> raise e
  | e -> raise(Error(Linking_error(Printexc.to_string e)))

(* Main entry point *)

(* [path_in_archive prefix str suffix] returns:
   - [wi ^ str2 ^ suffix] if [prefix = ""];
   - [wi ^ prefix2 ^ "/" ^ str2 ^ suffix] otherwise;
   where:
   - [str2] and [prefix2] are respectively [str] and [prefix] with
     ['.'] characters replaced with ['/'] characters;
   - [wi] is ["WEB-INF/classes/"] if linking a servlet, and [""] otherwise. *)
let path_in_archive prefix str suffix =
  let len = String.length str in
  let buff = Buffer.create (len + 24 + String.length prefix + String.length suffix) in
  let add_string buff str =
    let len = String.length str in
    for i = 0 to pred len do
      let ch = str.[i] in
      if ch = '.' then
        Buffer.add_char buff '/'
      else
        Buffer.add_char buff ch
    done in
  begin match !Jclflags.war with
  | Some _ -> Buffer.add_string buff "WEB-INF/classes/"
  | None -> ()
  end;
  if prefix <> "" then begin
    add_string buff prefix;
    Buffer.add_char buff '/'
  end;
  add_string buff str;
  Buffer.add_string buff suffix;
  Buffer.contents buff

let globals_map units =
  let units =
    List.map
      (fun (unit, _, crc) ->
        try
          (unit.Cmj_format.ui_name,
           (List.assoc unit.Cmj_format.ui_name unit.Cmj_format.ui_imports_cmi),
           crc,
           unit.Cmj_format.ui_defines)
        with Not_found ->
          raise (Error (Linking_error ("internal error in Javalink.globals_map"))))
      units in
  Marshal.to_string units []

module StringSet = Set.Make (String)

let link ppf objfiles output_name =
  ignore ppf;
  let stdlib = "stdlib" ^ Jconfig.ext_library in
  let objfiles =
    if !Clflags.nopervasives then objfiles
    else stdlib :: objfiles in
  let units_tolink = List.fold_right scan_file objfiles [] in
  Array.iter remove_required Runtimedef.builtin_exceptions;
  List.iter
    (fun (info, file_name, _crc) ->
      if info.Cmj_format.ui_ints_are_63_bit_long <> Jconfig.ints_are_63_bit_long
      then raise(Error(File_compiled_with_a_different_int_size file_name)))
    units_tolink;
  begin match extract_missing_globals() with
    [] -> ()
  | mg -> raise(Error(Missing_implementations mg))
  end;
  List.iter
    (fun (info, file_name, crc) -> check_consistency file_name info crc)
    units_tolink;
  let startup_name, startup_class =
    make_startup_class units_tolink output_name in
  try
    let builder = Archiveutils.open_builder output_name in
    (* always add startup class and globals map *)
    startup_class
    |> Archiveutils.add_entry builder (path_in_archive "" startup_name Jconfig.ext_class);
    units_tolink
    |> globals_map
    |> Bytes.make_of_string
    |> Archiveutils.add_entry builder (path_in_archive "" startup_name ".gmap");
    (* special linking for applets *)
    begin match !Jclflags.applet with
    | Some (Jclflags.Graphics | Jclflags.Awt | Jclflags.Swing) ->
        begin match List.rev units_tolink with
        | (last_unit, cmj_file, _) :: _ ->
            (* the last unit should have a signature coherent with the applet kind *)
            let signature_cmi, approx =
              try
                check_applet_signature last_unit cmj_file
              with Failure s ->
                raise (Error (Invalid_applet_signature s)) in
            let approx = extract_applet_approx signature_cmi (Array.to_list approx) in
            (* add a class that actually inherit from java.applet.Applet *)
            let applet_name, applet_class = make_applet_class last_unit approx in
            Archiveutils.add_entry
              builder
              (path_in_archive "" applet_name Jconfig.ext_class)
              applet_class
        | [] ->
            raise (Error (Linking_error ("internal error in Javalink.link")))
        end
    | None ->
        ()
    end;
    (* special linking for servlets *)
    begin match !Jclflags.war with
    | Some file ->
        let servlet_name, servlet_class = make_servlet_class () in
        Archiveutils.add_entry
          builder
          (path_in_archive "" servlet_name Jconfig.ext_class)
          servlet_class;
        let packages =
          List.fold_left
            (fun acc (info, _file_name, _crc) ->
              List.fold_left
                (fun acc elem -> StringSet.add elem acc)
                acc
                info.Cmj_format.ui_ensure_init)
            StringSet.empty
            units_tolink in
        StringSet.iter
          (fun package ->
            let servlet_name, servlet_class = make_servlet_aux_class package in
            Archiveutils.add_entry
              builder
              (path_in_archive "" servlet_name Jconfig.ext_class)
              servlet_class)
          (StringSet.remove !Jclflags.java_package packages);
        (try
          Archiveutils.add_entry_from_file
            builder
            "WEB-INF/web.xml"
            file
        with _ ->
          raise (Error (File_not_found file)))
    | None ->
        ()
    end;
    (* determine classpath *)
    let jars =
      let cmja_files =
        List.filter
          (fun file -> Filename.check_suffix file Jconfig.ext_library)
          objfiles in
      List.map
        (fun file -> (Filename.chop_suffix file Jconfig.ext_library) ^ Jconfig.ext_lib)
        cmja_files in
    let class_path =
      if !Jclflags.standalone then begin
        List.rev !Jclflags.additional_jar_refs
      end else begin
        [ Jconfig.runtime_support_jar ]
        @ jars
        @ (List.rev !Jclflags.additional_jars)
        @ (List.rev !Jclflags.additional_jar_refs)
      end in
    (* build manifest and add it to archive *)
    if !Jclflags.war = None then begin
      let manifest =
        { Manifest.default with
          Manifest.main_class = Some (Bytecodeutils.make_class startup_name);
          class_path = List.map UTF8.of_string class_path} in
      ArchiveBuilder.add_manifest builder manifest
    end;
    (* add runtime parameters to archive *)
    begin try
      !Jclflags.runtime_parameters
      |> Javalink_parameters.compile
      |> Archiveutils.add_entry
          builder
          (path_in_archive !Jclflags.java_package "" Jconfig.parameters_entry)
    with Failure s ->
      raise (Error (Invalid_runtime_parameter s))
    end;
    (* add contents of cmj files to archive *)
    objfiles
    |> List.filter (fun file -> Filename.check_suffix file Jconfig.ext_compiled)
    |> List.iter
        (fun cmj_file ->
          let cmj_file_in_path =
            try
              Misc.find_in_path
                !Config.load_path
                (Filename.chop_suffix cmj_file Jconfig.ext_compiled ^ Jconfig.ext_obj)
            with Not_found -> raise (Error (File_not_found cmj_file)) in
          let cmj_archive =
            cmj_file_in_path
            |> Path.make_of_string
            |> ArchiveFile.make_of_path in
          let prefix =
            match !Jclflags.war with
            | Some _ -> UTF8.of_string "WEB-INF/classes/"
            | None   -> UTF8.of_string "" in
          ArchiveFile.iter_entries
            (fun entry ->
              if not (ArchiveEntry.is_directory entry) then begin
                let data = ArchiveFile.bytes_of_entry cmj_archive entry in
                let entry_name = ArchiveEntry.get_filename entry in
                ArchiveBuilder.add_entry builder ~prefix entry_name data
              end)
            cmj_archive;
          ArchiveFile.close cmj_archive);
    (* if in standalone mode, merge referenced libraries into archive *)
    if !Jclflags.standalone then begin
      let service_prefix = UTF8.of_string "META-INF/services/" in
      let services = Hashtbl.create 17 in
      List.iter
        (fun jar_file ->
          let jar_file_in_path =
            try
              Misc.find_in_path !Config.load_path jar_file
            with Not_found ->
              raise (Error (File_not_found jar_file)) in
          let fname_prefix =
            match !Jclflags.war with
            | None   -> UTF8.of_string ""
            | Some _ -> UTF8.of_string "WEB-INF/classes/" in
          let jar_archive =
            jar_file_in_path
            |> Path.make_of_string
            |> ArchiveInputStream.make_of_path in
          ArchiveInputStream.iter_entries
            (fun entry bytes ->
              let fname = ArchiveEntry.get_filename entry in
              if not (ArchiveEntry.is_directory entry) then begin
                if UTF8.starts_with service_prefix fname then begin
                  let l = try Hashtbl.find services fname with Not_found -> [] in
                  Hashtbl.replace services fname (bytes :: l)
                end else if not (UTF8.equal fname Manifest.path_in_archive) then begin
                  ArchiveBuilder.add_entry builder (UTF8.(++) fname_prefix fname) bytes
                end else
                  ()
              end)
            jar_archive;
          ArchiveInputStream.close jar_archive)
        (Jconfig.runtime_support_jar :: jars @ !Jclflags.additional_jars);
      Hashtbl.iter
        (fun name l ->
          if !Jclflags.nomerge && (List.length l > 1) then
            raise (Error (Duplicate_entry (UTF8.to_string name)))
          else
            let data = ByteBuffer.make_of_size 1024 in
            let first = ref true in
            l
            |> List.rev
            |> List.iter
                (fun d ->
                  if !first then begin
                    ByteBuffer.add_byte data 10;
                    first := false
                  end;
                  ByteBuffer.add_bytes data d);
            data
            |> ByteBuffer.contents
            |> ArchiveBuilder.add_entry builder name)
        services
    end;
    (* add 'additional' classes to archive *)
    List.iter
      (fun file ->
        try
          let stream =
            file
            |> open_in_bin
            |> InputStream.make_of_channel in
          let def =
            stream
            |> ClassFile.read
            |> ClassDefinition.decode in
          let name =
            def.ClassDefinition.name
            |> Name.internal_utf8_for_class
            |> UTF8.to_string in
          Archiveutils.add_entry_from_file
            builder
            (name ^ Jconfig.ext_class) file;
          InputStream.close stream
        with _ ->
          raise (Error (Unable_to_add_class file)))
      !Jclflags.additional_classes;
    (* add 'additional' files to archive *)
    List.iter
      (fun file_spec ->
        try
          let file, path =
            try
              let idx = String.index file_spec '@' in
              (String.sub file_spec 0 idx),
              (String.sub file_spec (succ idx) ((String.length file_spec) - idx - 1))
            with Not_found ->
              file_spec, "" in
          let base = Filename.basename file in
          let name = if path = "" then base else path ^ "/" ^ base in
          Archiveutils.add_entry_from_file builder name file
        with _ ->
          raise (Error (Unable_to_add_file file_spec)))
      !Jclflags.additional_files;
    Archiveutils.close builder
  with
  | (Error _) as e -> raise e
  | e -> raise (Error (Linking_error (Printexc.to_string e)))

(* Error report *)

let report_error ppf = function
  | File_not_found name ->
      Format.fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      Format.fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           Format.fprintf ppf "%s" r1;
           List.iter (fun r -> Format.fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            Format.fprintf ppf "@ @[<hov 2>%s referenced from %a@]" md
            print_references rq) in
      Format.fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      Format.fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Inconsistent_implementation(intf, file1, file2) ->
      Format.fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %s@]"
       Location.print_filename file1 Location.print_filename file2 intf
  | Linking_error msg ->
      Format.fprintf ppf "Error during linking:@ %s" msg
  | Multiple_definition(modname, file1, file2) ->
      Format.fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %s@]"
        Location.print_filename file1
        Location.print_filename file2
        modname
  | Missing_cmj(filename, name) ->
      Format.fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmj file@ for module %s,@ \
         which was produced by `ocamljava -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %s.cmj@ is found.@]"
        Location.print_filename filename name
        Location.print_filename filename
        name
  | Unable_to_add_class filename ->
      Format.fprintf ppf "Unable to add class from file %s" filename
  | Unable_to_add_file filename ->
      Format.fprintf ppf "Unable to add file %s" filename
  | Duplicate_entry name ->
      Format.fprintf ppf "Duplicate jar entry (%s)" name
  | Invalid_applet_signature kind ->
      Format.fprintf ppf "Invalid %s applet signature" kind
  | File_compiled_with_a_different_int_size name ->
      Format.fprintf ppf "File %s@ has been compiled with a different int size" name
  | Invalid_runtime_parameter msg ->
      Format.fprintf ppf "Invalid runtime parameter:@ %s" msg
