(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/cmmgen.ml in the OCaml source
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
    Invalid_servlet_signature of string

exception Error of error

(* compilation of functions *)

module StringSet = Set.Make (struct
  type t = string
  let compare x y = String.compare x y
end)

type fundesc = {
    lbl : string;
    params : (Ident.t * Lambda.repr) list;
    repr : Lambda.repr;
    body : Jlambda.jlambda;
  }

let transl_function add_function { lbl; params; repr; body } =
  let is_entry = lbl = "entry" in
  { Macroinstr.fun_name = lbl;
    fun_args = List.map fst params;
    fun_params = List.map snd params;
    fun_return = repr;
    fun_body = Macrogen.translate add_function is_entry params repr body; }

let rec transl_all_functions take_function add_function already_translated cont =
  try
    let func = take_function () in
    if StringSet.mem func.lbl already_translated then
      transl_all_functions
        take_function
        add_function
        already_translated
        cont
    else
      transl_all_functions
        take_function
        add_function
        (StringSet.add func.lbl already_translated)
        (transl_function add_function func :: cont)
  with
  | Not_found -> cont


(* compilation of whole unit *)

let mk_seq e1 e2 =
  Macroinstr.Msequence (e1, e2)

(* adds init marks to be used by the ocamljar optimizer *)
let rec add_init_marks glob seen expr =
  let open Macroinstr in
  match expr with
  | Msequence ((Mwritelocal (Boxed_value, 1, _)) as e1,
               Msequence ((Mprim (Lambda.Psetfield (idx, _),
                                  [ Mprim (Lambda.Pgetglobal id, [], _);
                                    Mreadlocal (Boxed_value, 1) ],
                                  _debug_info)) as e2,
                          cont))
    when (glob = (Ident.name id)) && not (List.mem idx seen) ->
      let seen = idx :: seen in
      let seen, cont = add_init_marks glob seen cont in
      let seq =
        mk_seq (Minit (Some idx))
        @@ mk_seq e1
        @@ mk_seq e2
        @@ mk_seq (Minit None) cont in
      seen, seq
  | Msequence (e1, e2) ->
      let seen, e1 = add_init_marks glob seen e1 in
      let seen, e2 = add_init_marks glob seen e2 in
      seen, mk_seq e1 e2
  | _ ->
      seen, expr

let compunit current_class global_class jlam =
  let glob = Jcompilenv.make_symbol None in
  let name = Jcompilenv.make_symbol (Some "entry") in
  let functions : fundesc Queue.t = Queue.create () in
  let take_function () =
    try
      Queue.take functions
    with Queue.Empty ->
      raise Not_found in
  let add_function cls lbl params repr body =
    if cls = current_class then
      Queue.add { lbl; params; repr; body } functions in
  let repr = Lambda.LR_unit in
  let init_code = Macrogen.translate add_function true [] repr jlam in
  let _, init_code = add_init_marks glob [] init_code in
  let globals = Macroinstr.Mcreateglobal (glob, global_class) in
  let body =
    mk_seq
      (Macroinstr.Mwritelocal (Macroinstr.Boxed_value, 0, globals))
      init_code in
  let entry = { Macroinstr.fun_name = name;
                fun_args = [];
                fun_params = [];
                fun_return = repr;
                fun_body = body; } in
  transl_all_functions
    take_function
    add_function
    StringSet.empty
    [entry]

let main_return x =
  if !Jclflags.scripting then
    match x with
    | Jlambda.Jsequence (y, _) -> y
    | _ -> x
  else
    x

let path_of_class_name x =
  let res = String.copy x in
  for i = 0 to pred (String.length res) do
    if res.[i] = '.' then res.[i] <- '/'
  done;
  res

let compile_implementation prefixname ppf (size, lam) =
  Macrogen_global.reset_global_uses ();
  let print_if flag printer arg =
    if !flag then Format.fprintf ppf "%a@." printer arg in
  let current_class = Jcompilenv.current_class_name () in
  let global_class = Jcompilenv.current_global_class_name () in
  let class_path = path_of_class_name current_class in
  let jlam = main_return (Jclosure.intro size lam) in
  print_if Jclflags.dump_jlambda Printjlambda.jlambda jlam;
  let functions = compunit current_class global_class jlam in
  print_if Jclflags.dump_minstr Printmacroinstr.macroinstr functions;
  Bytecodegen_constants.reset ();
  let methods =
    functions
    |> List.map
        (fun fundecl ->
          let open Macroinstr in
          let impl = Bytecodegen.compile_function ppf fundecl in
          let unboxed_repr =
            List.exists
              (function
                | Lambda.LR_unit | Lambda.LR_none -> true
                | r -> Macroinstr.kind_of_repr r <> Macroinstr.Boxed_value)
              (fundecl.fun_return :: fundecl.fun_params) in
          if (fundecl.fun_name <> "entry") && unboxed_repr then
            (* If a parameter value or the return value is passed
               unboxed, a surrogate method has to be compiled, that
               uses only boxed representation and calls the original
               function (handling boxing/unboxing).

               The surrogate method has to appear first to be used by
               ocamlwrap (the two resulting methods may differ only
               regarding their result type, and in this case the Java
               language does not allow to disambiguate between the two). *)
            [ Bytecodegen.compile_surrogate fundecl ;
              impl ]
          else
            [ impl ])
    |> List.flatten in
  let createConstants_methods, needs_marshalled_data =
    Bytecodegen.compile_constants_to_methods ppf in
  let static_block, fields = Bytecodegen.compile_fields ppf in
  let get_global_method = Bytecodegen.compile_get_global_method global_class in
  let methods = static_block :: get_global_method :: (createConstants_methods @ methods) in
  let annotations = Macrogen_global.compile_global_uses () in
  let class_data = Bytecodegen.compile_class fields methods annotations in
  let global_class_data = Bytecodegen_global.compile_class () in
  let archive = Archiveutils.open_builder (prefixname ^ Jconfig.ext_obj) in
  Archiveutils.add_entry
    archive
    (class_path ^ Jconfig.ext_class)
    class_data;
  Archiveutils.add_entry
    archive
    ((path_of_class_name global_class) ^ Jconfig.ext_class)
    global_class_data;
  Bytecodegen_constants.compile_class needs_marshalled_data
  |> List.iter
      (fun (entry_name, entry_contents) ->
        Archiveutils.add_entry
          archive
          entry_name
          entry_contents);
  begin match !Jclflags.servlet with
  | Some kind ->
      (try
        let mod_kind =
          match kind with
          | Jclflags.Generic                     -> "Generic"
          | Jclflags.Http                        -> "HTTP"
          | Jclflags.Context_listener            -> "ServletContextListener"
          | Jclflags.Context_attribute_listener  -> "ServletContextAttributeListener"
          | Jclflags.Session_listener            -> "HTTPSessionListener"
          | Jclflags.Session_activation_listener -> "HTTPSessionActivationListener"
          | Jclflags.Session_attribute_listener  -> "HTTPSessionAttributeListener"
          | Jclflags.Session_binding_listener    -> "HTTPSessionBindingListener"
          | Jclflags.Session_id_listener         -> "HTTPSessionIdListener" in
        let signature_cmi =
          Jcompilenv.check_signature
            (Jcompilenv.current_unit_infos ()).Cmj_format.ui_name
            (prefixname ^ ".cmi")
            "JavaServlet"
            mod_kind in
        Archiveutils.add_entry
          archive
          (class_path ^ "Impl" ^ Jconfig.ext_class)
          (Javagen_servlet.compile kind mod_kind current_class signature_cmi)
      with Failure s ->
        raise (Error (Invalid_servlet_signature s)))
  | None -> ()
  end;
  Archiveutils.close archive

(* Error report *)

let report_error ppf = function
  | Invalid_servlet_signature kind ->
      Format.fprintf ppf "Invalid %s servlet signature" kind
