(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/compilenv.ml in the OCaml source
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
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string
  | Cannot_determine_class of string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, Cmj_format.unit_infos option) Hashtbl.t)

let current_unit =
  let open Cmj_format in
  { ui_name = "";
    ui_javaname = "";
    ui_additional_classes = [];
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmj = [];
    ui_approx = Jlambda.Value_unknown None;
    ui_force_link = false;
    ui_ensure_init = [];
    ui_ints_are_63_bit_long = Jconfig.ints_are_63_bit_long; }

let toplevel_infos_table =
  (Hashtbl.create 17 : (string, string) Hashtbl.t)

let register_toplevel_unit () =
  Hashtbl.add
    toplevel_infos_table
    current_unit.Cmj_format.ui_name
    current_unit.Cmj_format.ui_javaname

let toplevel_class id =
  try
    Some (Hashtbl.find toplevel_infos_table (Ident.name id))
  with _ ->
    None

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p ->
      let b = Buffer.create 64 in
      for i = 0 to String.length p - 1 do
        match p.[i] with
        | '.' -> Buffer.add_string b "__"
        |  c  -> Buffer.add_char b c
      done;
      Buffer.add_string b "__";
      Buffer.add_string b name;
      Buffer.contents b


let reset ?packname name =
  let open Cmj_format in
  Hashtbl.clear global_infos_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_javaname <- !Jclflags.java_package ^ "." ^ name;
  current_unit.ui_additional_classes <- [];
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmj <- [];
  current_unit.ui_approx <- Jlambda.Value_unknown None;
  current_unit.ui_force_link <- false;
  current_unit.ui_ensure_init <- if !Jclflags.servlet <> None then [!Jclflags.java_package] else []

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.Cmj_format.ui_name

let current_class_name () =
  !Jclflags.java_package ^ "." ^ current_unit.Cmj_format.ui_name

let current_global_class_name () =
  !Jclflags.java_package ^ "." ^ current_unit.Cmj_format.ui_name ^ "$Global"

let make_symbol ?(unitname = current_unit.Cmj_format.ui_symbol) idopt =
  match idopt with
  | None -> unitname
  | Some id -> id

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = Misc.input_bytes ic (String.length Jconfig.cmj_magic_number) in
    if buffer <> Jconfig.cmj_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : Cmj_format.unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = Misc.input_bytes ic (String.length Jconfig.cmja_magic_number) in
  if buffer <> Jconfig.cmja_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : Cmj_format.library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let cmj_not_found_crc =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let get_global_info register global_ident = (
  let open Cmj_format in
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        try
          let filename =
            Misc.find_in_path_uncap !Config.load_path (modname ^ Jconfig.ext_compiled) in
          let (ui, crc) = read_unit_info filename in
          if ui.ui_name <> modname then
            raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
          (Some ui, crc)
        with Not_found ->
          (None, cmj_not_found_crc) in
      if register then begin
        current_unit.ui_imports_cmj <-
          (modname, crc) :: current_unit.ui_imports_cmj;
        Hashtbl.add global_infos_table modname infos
      end;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.Cmj_format.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let toplevel_approx = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx current_unit.Cmj_format.ui_name current_unit.Cmj_format.ui_approx

let global_approx id =
  let open Cmj_format in
  if (Ident.name id) = current_unit.ui_symbol then current_unit.ui_approx
  else if Ident.is_predef_exn id then Jlambda.Value_unknown None
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info true id with
    | None -> Jlambda.Value_unknown None
    | Some ui -> ui.ui_approx

let get_last_component id =
  let id = Ident.name id in
  let len = String.length id in
  let i = ref (pred len) in
  while (!i >= 2) && ((id.[!i - 1] <> '_') || (id.[!i - 2] <> '_')) do
    decr i
  done;
  if (!i >= 2) then
    let res = String.sub id !i (len - !i) in
    Some (Ident.create res)
  else
    None

let global_approx_no_dep id =
  let open Cmj_format in
  if (Ident.name id) = current_unit.ui_symbol then
    current_unit.ui_approx
  else if Ident.is_predef_exn id then
    Jlambda.Value_unknown None
  else
    try
      Hashtbl.find toplevel_approx (Ident.name id)
    with Not_found ->
      match get_global_info false id with
      | Some ui -> ui.ui_approx
      | None ->
          begin match get_last_component id with
          | Some id' ->
              begin match get_global_info false id' with
              | Some ui -> ui.ui_approx
              | None -> Jlambda.Value_unknown None
              end
          | None -> Jlambda.Value_unknown None
          end

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef_exn id then
    "caml_exn_" ^ Ident.name id
  else begin
    match get_global_info true id with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.Cmj_format.ui_symbol None
  end

let class_for_global id =
  let open Cmj_format in
  let error () =
    raise (Error (Cannot_determine_class (Ident.name id))) in
  if (Ident.name id) = current_unit.ui_symbol then
    current_unit.ui_javaname
  else begin
    match toplevel_class id with
    | Some x -> x
    | None ->
        begin match get_global_info false id with
        | Some ui -> ui.ui_javaname
        | None ->
            begin match get_last_component id with
            | Some id' ->
                begin match get_global_info false id' with
                | Some ui -> ui.ui_javaname
                | None -> error ()
                end
            | None -> error ()
            end
        end
  end

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  current_unit.Cmj_format.ui_approx <- approx

let get_global_approx () =
  current_unit.Cmj_format.ui_approx

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc Jconfig.cmj_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.Cmj_format.ui_imports_cmi <- Env.imported_units ();
  write_unit_info current_unit filename

let check_signature module_name cmi_file sign_mod sign_submod =
  try
    let env = Env.initial in
    let env =
      if !Clflags.nopervasives then
        env
      else
        Env.open_pers_signature "Pervasives" env in
    let env = Env.open_pers_signature "JavaApplet" env in
    let env = Env.open_pers_signature "JavaFX" env in
    let signature_cmi = Env.read_signature module_name cmi_file in
    let path = Longident.(Ldot ((Lident sign_mod), sign_submod)) in
    let signature_appl = Env.lookup_modtype path env in
    let signature_appl =
      match signature_appl with
      | _, Types.Modtype_manifest (Types.Mty_signature sign) -> sign
      | _ -> raise Not_found in
    ignore (Includemod.signatures env signature_cmi signature_appl);
    signature_cmi
  with _ ->
    failwith (sign_mod ^ "." ^ sign_submod)

(* Error report *)

let report_error ppf = function
  | Not_a_unit_info filename ->
      Format.fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      Format.fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming (name, modname, filename) ->
      Format.fprintf ppf "%a@ contains the description for unit\
                          @ %s when %s was expected"
        Location.print_filename filename name modname
  | Cannot_determine_class symbol ->
      Format.fprintf ppf "Cannot determine class for symbol %s" symbol
