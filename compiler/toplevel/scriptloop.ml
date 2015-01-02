(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (toplevel/opttoploop.ml in the OCaml source
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

open Config
open Parsetree
open Misc
open Format


type res = Ok of Obj.t | Err of string
let _ = Ok (Obj.repr 0), Err "" (* to avoid warning, as value are not created by OCaml code *)
type evaluation_outcome = Result of Obj.t | Exception of exn

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"
external ndl_loadsym: string -> Obj.t = "caml_natdynlink_loadsym"

let _need_symbol sym =
  try ignore (ndl_loadsym sym); false
  with _ -> true

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry)) with exn -> Exception exn) with
    | Exception _ as r -> r
    | Result r ->
        match Obj.magic r with
          | Ok x -> Result x
          | Err s -> fatal_error ("Scriptloop.dll_run " ^ s)


let phrase_seqid = ref 0
let phrase_name = ref "SCRIPT"

let load_lambda (size, lam) =
  let slam = Simplif.simplify_lambda lam in

  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ Jconfig.ext_obj
    else Filename.temp_file ("caml" ^ !phrase_name) Jconfig.ext_obj
  in
  let fn = Filename.chop_extension dll in
  Javagen.compile_implementation fn err_formatter (size, slam);
  Jcompilenv.register_toplevel_unit ();
  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  let res = dll_run dll (!Jclflags.java_package ^ "." ^ !phrase_name) in
  (try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  res

let toplevel_env = ref Env.empty

let execute_phrase : Parsetree.toplevel_phrase -> 'a = function
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      incr phrase_seqid;
      phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
      Jcompilenv.reset ?packname:None !phrase_name;
      Typecore.reset_delayed_checks ();
      let (str, _sg, newenv) = Typemod.type_structure oldenv sstr Location.none in
      Typecore.force_delayed_checks ();
      let res = Translmod.transl_store_phrases !phrase_name str in
      begin try
        toplevel_env := newenv;
        let res = load_lambda res in
        match res with
        | Result v ->
            Jcompilenv.record_global_approx_toplevel ();
            Obj.magic v
        | Exception exn ->
            toplevel_env := oldenv;
            if exn = Out_of_memory then Gc.full_major();
            raise exn
      with x ->
        toplevel_env := oldenv; raise x
      end
  | Ptop_dir _ ->
      Obj.magic ()

let got_eof = ref false

let read_input_default buffer len =
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char Pervasives.stdin in
      buffer.[!i] <- c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
  | End_of_file ->
      (!i, true)
  | Exit ->
      (!i, false)

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let (len, eof) = read_input_default buffer len in
    if eof then begin
      Location.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end


let _ =
  Sys.interactive := true;
  Dynlink.init ();
  Compmisc.init_path true;
  Clflags.dlcode := true;
  ()

let set_paths () =
  load_path := !load_path @ [Filename.concat Config.standard_library "camlp4"];
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  ()

let initialize_toplevel_env () =
  toplevel_env := initial_env()

let eval : string -> 'a = fun s ->
  (* init *)
  let lb = Lexing.from_string s in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  (* eval *)
  let snap = Btype.snapshot () in
  try
    let phrases = Parse.use_file lb in
    List.fold_left
      (fun _ phrase ->
        execute_phrase phrase)
      (Obj.magic ())
      phrases
  with
  | e ->
      Btype.backtrack snap;
      let buf = Buffer.create 128 in
      let fmt = Format.formatter_of_buffer buf in
      (try Javaerrors.report_error fmt e with _ -> failwith "unable to evaluate script");
      Format.pp_print_flush fmt ();
      failwith (Buffer.contents buf)

let directory : string -> unit = fun s ->
  let d = expand_directory Config.standard_library s in
  Config.load_path := d :: !Config.load_path

let rec load_file ppf name =
  let filename = try Some (find_in_path !Config.load_path name) with Not_found -> None in
  match filename with
  | None -> fprintf ppf "Cannot find file %s.@." name; false
  | Some name -> really_load_file ppf name

and really_load_file ppf filename =
  let fn,tmp =
    if Filename.check_suffix filename ".cmj" || Filename.check_suffix filename ".cmja"
    then
      let cmjs = Filename.temp_file "caml" ".cmjs" in
      Javalink.link_shared std_formatter [filename] cmjs;
      cmjs,true
    else
      filename,false in
  let success =
    (* The Dynlink interface does not allow us to distinguish between
       a Dynlink.Error exceptions raised in the loaded modules
       or a genuine error during dynlink... *)
    try Dynlink.loadfile fn; true
    with
      | Dynlink.Error err ->
          fprintf ppf "Error while loading %s: %s.@."
            filename (Dynlink.error_message err);
          false
      | exn ->
          (try Javaerrors.report_error ppf exn with _ -> ());
          false
  in
  if tmp then (try Sys.remove fn with Sys_error _ -> ());
  success

let load : string -> unit = fun s ->
  ignore (load_file Format.err_formatter s)

let init () =
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "//script//";
  Location.input_lexbuf := Some lb;
  Callback.register "ocamljava javax.script.eval" eval;
  Callback.register "ocamljava javax.script.directory" directory;
  Callback.register "ocamljava javax.script.load" load
