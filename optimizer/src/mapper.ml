(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java optimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java optimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open BaristaLibrary


let list cond l = if cond then l else []

let pushes_int_to_remove remove_index instr =
  let pushed =
    match instr with
    | Instruction.ICONST_M1      -> -1
    | Instruction.ICONST_0       -> 0
    | Instruction.ICONST_1       -> 1
    | Instruction.ICONST_2       -> 2
    | Instruction.ICONST_3       -> 3
    | Instruction.ICONST_4       -> 4
    | Instruction.ICONST_5       -> 5
    | Instruction.BIPUSH x       -> (x :> int)
    | Instruction.SIPUSH x       -> (x :> int)
    | Instruction.LDC_W (`Int x) -> Int32.to_int x
    | _                          -> -1 in
  if pushed >= 0 then
    remove_index !State.current_class_name pushed
  else
    false

let rec nops n l =
  if n <= 0 then
    l
  else
    nops (pred n) (Instruction.NOP :: l)

(* cannot be done by a peephole rule, because the code between markers
  can contain jumps *)
let remove_unused_globals remove_index code =
  (* nops are inserted to keep offsets contant *)
  let rec rewrite ofs keep acc = function
    | int_instr
      :: ((Instruction.INVOKESTATIC (cn, mn, ([`Int], `Void))) as call_instr)
      :: tl
      when (Name.equal_for_class cn Names.abstract_native_runner)
          && (Name.equal_for_method mn Names.init_global_begin) ->
        let int_sz = Instruction.size_of ofs int_instr in
        let call_sz = Instruction.size_of (ofs + int_sz) call_instr in
        let sz = int_sz + call_sz in
        let keep =
          if pushes_int_to_remove remove_index int_instr then
            false
          else
            keep in
        rewrite (ofs + sz) keep (nops sz acc) tl
    | Instruction.INVOKESTATIC (cn, mn, ([], `Void))
      :: tl
      when (Name.equal_for_class cn Names.abstract_native_runner)
          && (Name.equal_for_method mn Names.init_global_end) ->
        if not keep then
          let push_unit =
            Instruction.GETSTATIC (Names.value,
                                   Names.unit,
                                   `Class Names.value) in
          rewrite (ofs + 3) true (push_unit :: acc) tl
        else
          rewrite (ofs + 3) true (nops 3 acc) tl
    | hd :: tl ->
        let sz = Instruction.size_of ofs hd in
        if keep then
          rewrite (ofs + sz) keep (hd :: acc) tl
        else
          rewrite (ofs + sz) keep (nops sz acc) tl
    | [] -> List.rev acc in
  rewrite 0 true [] code

(* cannot be done by a peephole rule, because the code between markers
  can contain jumps *)
let remove_debug code =
  let rec rewrite ofs keep acc = function
    | (Instruction.INVOKESTATIC (cn, mn, ([], `Void))) :: tl
      when (Name.equal_for_class cn Names.debug)
        && (Name.equal_for_method mn Names.begin_) ->
          rewrite (ofs + 3) false (nops 3 acc) tl
    | (Instruction.INVOKESTATIC (cn, mn, ([], `Void))) :: tl
      when (Name.equal_for_class cn Names.debug)
        && (Name.equal_for_method mn Names.end_) ->
          rewrite (ofs + 3) true (nops 3 acc) tl
    | hd :: tl ->
        let sz = Instruction.size_of ofs hd in
        if keep then
          rewrite (ofs + sz) keep (hd :: acc) tl
        else
          rewrite (ofs + sz) keep (nops sz acc) tl
    | [] -> List.rev acc in
  rewrite 0 true [] code

let optimize_code preopt_fun code_attr rules meth =
  let code, _, exception_table, graph =
    ControlFlow.graph_of_instructions
      (match preopt_fun with
      | Some f -> f code_attr.Attribute.code
      | None   ->   code_attr.Attribute.code)
      code_attr.Attribute.exception_table
    (* rules from Barista are not used here in order to avoid any
       "interference" (e.g. deleting just-inserted nops) *)
    |> Code.optimize_graph ~rules
    (* nops are removed in a separate phase because they are used as
       markers by some rules in order to ensure termination *)
    |> Code.optimize_graph ~rules:[ Rewrite.remove_nops ]
    |> Code.flatten_graph ~use_offsets:true in
  let unifier = State.get_unifier () in
  let max_stack, max_locals, stack_map_frame =
    Code.compute_stack_infos
      !State.current_class_name
      unifier
      graph
      (StackState.make_of_method !State.current_class_name meth) in
  let attributes =
    list (stack_map_frame <> []) [`StackMapTable stack_map_frame] in
  Attribute.({ max_stack; max_locals; code; exception_table; attributes; })

let apply_rules_for_constructor preopt_fun rules cm =
  if (rules <> []) || (preopt_fun <> None) then begin
    let code =
      Attribute.extract_code
        (cm.Method.cstr_attributes :> Attribute.t list) in
    let code = optimize_code preopt_fun code rules (Method.Constructor cm) in
    let cstr_attributes =
      List.map
        (function
          | `Code _ -> `Code code
          | x       -> x)
        cm.Method.cstr_attributes in
    { cm with Method.cstr_attributes }
  end else
    cm

let apply_rules_for_initializer preopt_fun rules im =
  if (rules <> []) || (preopt_fun <> None) then begin
    let code =
      Attribute.extract_code
        (im.Method.init_attributes :> Attribute.t list) in
    let code = optimize_code preopt_fun code rules (Method.Initializer im) in
    let init_attributes =
      List.map
        (function
          | `Code _ -> `Code code
          | x       -> x)
        im.Method.init_attributes in
    { im with Method.init_attributes }
  end else
    im

(* used for regular methods, as they can be abstract
   (and hence have no code attribute). *)
let extract_code_option rm =
  try
    Some (Attribute.extract_code (rm.Method.attributes :> Attribute.t list))
  with
  | Not_found -> None

let apply_rules_for_regular preopt_fun rules rm =
  if (rules <> []) || (preopt_fun <> None) then begin
    match extract_code_option rm with
    | Some code ->
        let code = optimize_code preopt_fun code rules (Method.Regular rm) in
        let attributes =
          List.map
            (function
              | `Code _ -> `Code code
              | x       -> x)
            rm.Method.attributes in
        { rm with Method.attributes }
    | None ->
        rm
  end else
    rm

class for_compiled_module remove_index = object

  inherit ClassTraversal.default_class_definition_mapper

  method! class_fields l =
    if !Args.one_context then begin
      let cn = !State.current_class_name in
      let l =
        List.fold_left
          (fun acc elem ->
            if Name.equal_for_field elem.Field.name Names.globals then
              let descriptor = `Class (Misc.global_class_of_class cn) in
              { elem with Field.descriptor } :: acc
            else if Name.equal_for_field elem.Field.name Names.constants then
              acc
            else if Misc.is_shared_constant elem.Field.name then begin
              WholeProgram.add_shared_constant elem.Field.name;
              acc
            end else
              elem :: acc)
          []
          l in
      let l' =
        List.map
          (fun f ->
            { f with Field.flags = `Static :: `Final :: f.Field.flags })
        (WholeProgram.additional_fields cn) in
      (List.rev l) @ l'
    end else
      l

  method! initializer_method im =
    let rules =
      list
        !Args.one_context
        [ Rewrite.constant_loading ;
          Rewrite.remove_shared_constants ;
          Rewrite.use_shared_constants ] in
    apply_rules_for_initializer None rules im

  method! regular_method rm =
    let is_create_constants =
      Name.equal_for_method rm.Method.name Names.create_constants in
    let is_entry =
      Name.equal_for_method rm.Method.name Names.entry in
    let rules =
      (list
         !Args.one_context
         [ Rewrite.accesses_to_globals_and_constants ;
           Rewrite.access_context_through_fields ;
           Rewrite.use_shared_constants ])
      @ (list
           !Args.no_runtime_lock
           [ Rewrite.remove_blocking_sections ])
      @ (list
           !Args.no_signals
           [ Rewrite.remove_signals ])
      @ (list
           is_create_constants
           [ Rewrite.create_constants ])
      @ (list
           (is_entry && !Args.no_unused_global)
           [ Rewrite.remove_unused_globals ]) in
    let remove_index =
      if is_entry && !Args.no_unused_global then
        Some remove_index
      else
        None in
    let res =
      apply_rules_for_regular
        (match remove_index with
        | Some f -> Some (remove_unused_globals f)
        | None   -> None)
        rules rm in
    if is_create_constants then
      { res with Method.descriptor = ([], `Void); }
    else
      res

end

class for_entry_point = object

  inherit ClassTraversal.default_class_definition_mapper

  method! regular_method rm =
    let rules =
      if Name.equal_for_method rm.Method.name Names.main_scripting then
        list !Args.one_context [Rewrite.remove_load_constant]
      else if Name.equal_for_method rm.Method.name Names.main_with_return then
        list !Args.one_context [Rewrite.remove_load_constant]
      else if Name.equal_for_method rm.Method.name Names.module_main then
        list !Args.no_dynlink [Rewrite.simplify_calls_to_entry]
      else
        [] in
    apply_rules_for_regular None rules rm

end

class for_runtime is_exception = object

  inherit ClassTraversal.default_class_definition_mapper

  val rules =
    (list !Args.one_context [Rewrite.access_context_through_fields])
    @ (list !Args.unsafe [Rewrite.safe_to_unsafe])
    @ (list !Args.no_runtime_lock [Rewrite.remove_blocking_sections])
    @ (list !Args.no_signals [Rewrite.remove_signals])

  val preopt_fun = if !Args.no_debug then Some remove_debug else None

  method! constructor_method cm =
    apply_rules_for_constructor preopt_fun rules cm

  method! initializer_method im =
    apply_rules_for_initializer preopt_fun rules im

  method! regular_method rm =
    if is_exception
        && (Name.equal_for_method rm.Method.name Names.fill_in_stack_trace)
        && ((fst rm.Method.descriptor) = [])
        && !Args.no_backtrace then
      let code = Attribute.({ max_stack = Utils.u2 1;
                              max_locals = Utils.u2 1;
                              code = [ Instruction.ALOAD_0 ;
                                       Instruction.ARETURN ] ;
                              exception_table = [];
                              attributes = []; }) in
      let code = optimize_code preopt_fun code rules (Method.Regular rm) in
      let attributes =
        List.map
          (function
            | `Code _ -> `Code code
            | x       -> x)
          rm.Method.attributes in
      { rm with Method.attributes }
    else
      apply_rules_for_regular preopt_fun rules rm

end

exception Ignore_class

class for_archive arch remove_index = object

  inherit ArchiveTraversal.default_archive_mapper arch as super

  val for_compiled_module = new for_compiled_module remove_index

  val for_entry_point = new for_entry_point

  val for_runtime = new for_runtime

  method! entry x =
    try
      super#entry x
    with Ignore_class ->
      ArchiveTraversal.Directory_entry x (* actually a no-op *)

  method! class_definition cd =
    State.current_class_name := cd.ClassDefinition.name;
    let is_compiled_module =
      List.exists
        (fun cn -> Name.equal_for_class cn Names.ocamljava_module)
        cd.ClassDefinition.implements in
    let annotations =
      Attribute.extract_annotations
        (cd.ClassDefinition.attributes :> Attribute.t list) in
    let is_entry_point =
      List.exists
        (fun (n, ev) ->
          if Name.equal_for_class n Names.entry_point then begin
            List.iter
              (fun (k, v) ->
                if UTF8.equal Names.standalone k then
                  match v with
                  | Annotation.Boolean_value true -> ()
                  | Annotation.Boolean_value false ->
                      prerr_endline "WARNING: the program was not linked in standalone mode"
                  | _ -> ())
              ev;
            true
          end else
            false)
        annotations in
    let is_constants =
      List.exists
        (fun (n, _) -> Name.equal_for_class n Names.constants_class)
        annotations in
    let is_runtime_class = Misc.is_runtime_class cd.ClassDefinition.name in
    let cn =
      cd.ClassDefinition.name
      |> Name.external_utf8_for_class
      |> UTF8.to_string in
    let verbose x =
      if !Args.verbose then print_endline x in
    if is_compiled_module then begin
      verbose (Printf.sprintf "optimizing class %S (compiled module)" cn);
      ClassDefinition.map for_compiled_module cd
    end else if is_entry_point then begin
      verbose (Printf.sprintf "optimizing class %S (entry point)" cn);
      ClassDefinition.map for_entry_point cd
    end else if is_runtime_class then begin
      verbose (Printf.sprintf "optimizing class %S (runtime library)" cn);
      let is_exception = Name.equal_for_class cd.ClassDefinition.name Names.fail_exception in
      ClassDefinition.map (for_runtime is_exception) cd
    end else if is_constants then begin
      verbose (Printf.sprintf "dropping class %S" cn);
      raise Ignore_class
    end else begin
      verbose (Printf.sprintf "skipping class %S" cn);
      cd
    end

end
