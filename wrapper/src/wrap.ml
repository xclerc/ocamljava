(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


open Wrap_common

let type_parameters_of_type_declaration type_declaration =
  let open Types in
  let rec map idx acc = function
    | hd :: tl ->
        (match hd.desc with
        | Tvar (Some id) -> map (succ idx) ((id, idx) :: acc) tl
        | _ -> map idx acc tl)
    | [] -> List.rev acc in
  map 0 [] type_declaration.type_params

let type_declaration name type_declaration =
  Output.verbose (Printf.sprintf "  wrapping type %S..." (Ident.name name));
  let open Types in
  let type_parameters = type_parameters_of_type_declaration type_declaration in
  match type_declaration.type_kind with
  | Type_abstract ->
      (match type_declaration.type_manifest with
      | Some { desc = Tvariant rd; _ } -> (* Polymorphic variant *)
          if not rd.row_closed then
            fail (Cannot_translate_open_polymorphic_variant name);
          let row_fields =
            List.filter
              (fun (_, field) ->
                match field with
                | Rpresent _ -> true
                | _ -> false)
              rd.row_fields in
          let constructors =
            List.map
              (fun (lbl, field) ->
                let typ = match field with
                | Rpresent (Some { desc = Ttuple l; _ }) -> l
                | Rpresent (Some te) -> [te]
                | Rpresent None -> []
                | _ ->
                    fail (Cannot_translate_polymorphic_variant name) in
                lbl, typ)
              row_fields in
          Wrap_variant.wrap
            name constructors Wrap_variant.Polymorphic_variant type_parameters
      | _ -> Wrap_abstract.wrap name type_parameters)
  | Type_record (fields, record_repr) ->
      Wrap_record.wrap name fields record_repr type_parameters
  | Type_variant constructors ->
      let constructors =
        List.map
          (fun (id, l, _) -> Ident.name id, l)
          constructors in
      Wrap_variant.wrap name constructors Wrap_variant.Simple_variant type_parameters

type wrapped_signature =
  | Method of JavaAST.method_
  | Class of JavaAST.class_

module StringMap = Map.Make (String)

let class_types : (Ident.t * Types.class_type_declaration) StringMap.t ref =
  ref StringMap.empty

let module_types : (Ident.t * Types.signature * (string list)) list ref = ref []

let signature_item modname sigitem approx approx_idx =
  let open Types in
  match sigitem with
  | Sig_value (name,
               { val_type = { desc = (Tarrow _); _ } as type_expr;
                 val_kind = Val_reg;
                 _ }) ->
     let m = Wrap_arrow.wrap modname name type_expr approx approx_idx in
     (match Wrap_arrow.wrap_closure modname name type_expr approx approx_idx with
     | Some c -> [ Method m ; Method c ]
     | None -> [ Method m ])
  | Sig_value (name,
               { val_type = { desc = (Tconstr _ | Ttuple _); _ } as type_expr;
                 val_kind = Val_reg;
                 _ }) ->
      if is_unit type_expr then begin
        Output.warning (Output.Discard_unit_value name);
        []
      end else
        [ Method (Wrap_value.wrap modname name type_expr approx_idx) ]
  | Sig_value (name,
               { val_type = { desc = (Tobject _); _ };
                 val_kind = Val_reg;
                 _ }) ->
      Output.warning (Output.Discard_object_value name);
      []
  | Sig_value (name,
               { val_type = { desc = (Tvariant _); _ };
                 val_kind = Val_reg;
                 _ }) ->
      Output.warning (Output.Discard_polymorphic_variant_value name);
      []
  | Sig_value (name, _) ->
      Output.warning (Output.Discard_unsupported_value name);
      []
  | Sig_type (name,
              ({ type_kind = Type_abstract;
                 type_manifest = Some synonym;
                 _ } as type_decl),
              _)
    when (not_an_object type_decl.type_manifest)
        && (match synonym.desc with Tconstr _ -> true | _ -> false) ->
      Output.verbose (Printf.sprintf "  type %S is just a synonym..." (Ident.name name));
      []
  | Sig_type (name, type_decl, _) when not_an_object type_decl.type_manifest ->
      [ Class (type_declaration name type_decl) ]
  | Sig_type (name, _, _) ->
      let n = Ident.name name in
      if ((n <> "") && (n.[0] = '#')) then
        []
      else if (StringMap.mem n !class_types) then begin
        let orig_name, class_type_declaration = StringMap.find n !class_types in
        if (Ident.name orig_name) = n then
          let abstract, final = Wrap_class_type.wrap name class_type_declaration in
          class_types := StringMap.remove n !class_types;
          [ Class abstract; Class final ]
        else begin
          Output.warning (Output.Discard_unsupported_type name);
          []
        end
      end else begin
        Output.warning (Output.Discard_unsupported_type name);
        []
      end
  | Sig_exception (name, exn_decl) ->
      [ Class (Wrap_exception.wrap name exn_decl.exn_args approx_idx) ]
  | Sig_module (name, ((Mty_functor _) as f), Trec_not) ->
      let res, params, eqs = flatten_functor !module_types f in
      [ Method (Wrap_functor.wrap modname name res params eqs !module_types approx approx_idx) ]
  | Sig_module (name, _, _) ->
      Output.warning (Output.Discard_nested_module name);
      []
  | Sig_modtype (name, Modtype_manifest (Mty_signature signature)) ->
      let abstract, final, parameters = Wrap_module_type.wrap name signature in
      module_types := (name, signature, parameters) :: !module_types;
      [ Class abstract; Class final ]
  | Sig_modtype (name, _) ->
      Output.warning (Output.Discard_nested_module name);
      []
  | Sig_class (name, _, _) ->
      Output.warning (Output.Discard_object_type name);
      []
  | Sig_class_type (name, class_type_declaration, _) ->
      class_types := StringMap.add (Ident.name name) (name, class_type_declaration) !class_types;
      []

let has_approx sigitem =
  let open Types in
  match sigitem with
  | Sig_value _
  | Sig_exception _
  | Sig_module _
  | Sig_class _ -> true
  | _ -> false

let make_class package_name cn meths inner =
  let open JavaAST in
  let empty_constr =
    constructor
      ~javadoc:["No instance of this class."]
      [Private] cn [] in
  let static_block = match !Wrap_common.main_static_block with
  | _ :: _ -> Some !Wrap_common.main_static_block
  | [] -> None in
  Wrap_common.clear_static_block ();
  { class_package = package_name;
    class_imports = [ "org.ocamljava.runtime.values.BlockValue";
                      "org.ocamljava.runtime.values.Value";
                      "org.ocamljava.runtime.kernel.AbstractNativeRunner";
                      "org.ocamljava.runtime.kernel.Fail";
                      "org.ocamljava.runtime.kernel.FailException";
                      "org.ocamljava.runtime.kernel.NativeApply";
                      "org.ocamljava.runtime.wrappers.*" ];
    class_modifiers = [Public; Final];
    class_name = cn;
    class_extends = None;
    class_fields = [];
    class_static_block = static_block;
    class_methods = (empty_constr :: meths);
    class_inner_classes = inner; }

let file cmi_filename package_name =
  let cmi_filename =
    try
      Misc.find_in_path !Config.load_path cmi_filename
    with _ -> fail (Cannot_find_cmi_file cmi_filename) in
  Output.verbose (Printf.sprintf "processing file %S..." cmi_filename);
  let filename_without_suffix = Filename.chop_suffix cmi_filename ".cmi" in
  let basename_without_suffix = Filename.basename filename_without_suffix in
  let module_name = String.capitalize basename_without_suffix in
  let signature =
    try
      Env.read_signature module_name cmi_filename
    with _ ->
      fail (Invalid_cmi_file cmi_filename) in
  State.update_environment_with_signature signature;
  let cmj_filename = filename_without_suffix ^ ".cmj" in
  let approx =
    if Sys.file_exists cmj_filename then begin
      let unit_info, _ =
        try
          Jcompilenv.read_unit_info cmj_filename
        with _ -> fail (Invalid_cmj_file cmj_filename) in
      State.java_class := Some unit_info.Cmj_format.ui_javaname;
      match unit_info.Cmj_format.ui_approx with
      | Jlambda.Value_tuple x -> Some x
      | _ -> fail (Invalid_cmj_file cmj_filename)
    end else begin
      State.java_class := None;
      None
    end in
  let _, wrapped =
    List.fold_left
      (fun (acc_idx, acc_res) elem ->
        let elem_approx =
          match has_approx elem, approx with
          | true, Some app -> Some app.(acc_idx)
          | _ -> None in
        let w = signature_item module_name elem elem_approx acc_idx in
        let acc_idx = if has_approx elem then succ acc_idx else acc_idx in
        (acc_idx, w :: acc_res))
      (0, [])
      signature in
  let wrapped = List.flatten wrapped in
  let class_name = Args.class_of_module module_name in
  let methods, inner_classes =
    List.fold_left
      (fun (acc_meths, acc_classes) elem ->
        match elem with
        | Method m -> m :: acc_meths, acc_classes
        | Class c -> acc_meths, (JavaAST.Full_class c) :: acc_classes)
      ([], [])
      wrapped in
  (match !Args.library_init with
  | Args.Explicit -> ()
  | Args.Static ->
      let lock =
        JavaAST.Identifier ("org.ocamljava.runtime.kernel.OCamlJavaThread.class") in
      let cond =
        JavaAST.(Infix
                   ("==",
                    Static_call
                      ("org.ocamljava.runtime.kernel.OCamlJavaThread",
                       "getCodeRunner",
                       []),
                    Null_literal)) in
      let pack_name =
        match !Args.library_package with
        | Some x -> x
        | None ->
            Wrap_common.fail
              (Wrap_common.Command_line_inconsistency "no library package name") in
      let args =
        match !Args.library_args with
        | Some x -> x
        | None -> "new String[0]" in
      Wrap_common.add_static_block
        [ JavaAST.(Synchronized_block
                     (lock,
                      [ If (cond,
                            Expression
                              (Static_call (pack_name ^ ".ocamljavaMain",
                                            "mainWithReturn",
                                            [ Identifier args ]))) ] )) ]);
  let class_ = make_class package_name class_name methods inner_classes in
  let class_ = JavaAST.add_unchecked_class class_ in
  let buffer = Buffer.create 4096 in
  JavaAST.dump_class buffer class_;
  let java_filename = class_name ^ ".java" in
  let chan = open_out java_filename in
  output_string chan (Buffer.contents buffer);
  close_out_noerr chan;
  TypeMap.promote module_name
