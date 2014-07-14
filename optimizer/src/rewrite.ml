(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2014 Xavier Clerc.
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


let remove_signals l =
  let rec rewrite acc = function
    | (_, Instruction.INVOKESTATIC (cn, mn, ([], `Void))) :: tl
      when (Name.equal_for_class Names.abstract_native_runner cn)
          && (Name.equal_for_method Names.check_signals mn) ->
        rewrite acc tl
    | (line, Instruction.INVOKESTATIC (cn, mn, md)) :: tl
      when Name.equal_for_class Names.signal_support cn ->
        let call = Instruction.INVOKESTATIC (Names.no_signal_support, mn, md) in
        let acc = (line, call) :: acc in
        rewrite acc tl
    | (line, Instruction.GETSTATIC (cn, fn, ft)) :: tl
      when Name.equal_for_class Names.signal_support cn ->
        let access = Instruction.GETSTATIC (Names.no_signal_support, fn, ft) in
        let acc = (line, access) :: acc in
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let is_enter_or_leave_blocking_section mn =
  List.exists
    (fun x -> Name.equal_for_method mn x)
    [ Names.enter_blocking_section ;
      Names.leave_blocking_section ]

let remove_blocking_sections l =
  let rec rewrite acc = function
    | (_, Instruction.INVOKESTATIC (cn, mn, ([], `Void))) :: tl
      when (Name.equal_for_class Names.current_context cn)
          && (is_enter_or_leave_blocking_section mn) ->
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let context_map = [
  (Misc.make_method_name "get",
   Misc.make_field_name "CONTEXT") ;

  (Misc.make_method_name "getParameters",
   Misc.make_field_name "PARAMETERS") ;

  (Misc.make_method_name "getCodeState",
   Misc.make_field_name "CODE_STATE") ;

  (Misc.make_method_name "getThreadsState",
   Misc.make_field_name "THREADS_STATE") ;

  (Misc.make_method_name "getFilesState",
   Misc.make_field_name "FILES_STATE") ;

  (Misc.make_method_name "getGCState",
   Misc.make_field_name "GC_STATE") ;

  (Misc.make_method_name "getFinalizersState",
   Misc.make_field_name "FINALIZERS_STATE") ;

  (Misc.make_method_name "getDebuggerState",
   Misc.make_field_name "DEBUGGER_STATE") ;

  (Misc.make_method_name "getSignalsState",
   Misc.make_field_name "SIGNALS_STATE") ;

  (Misc.make_method_name "getPredefinedExceptions",
   Misc.make_field_name "PREDEFINED_EXCEPTIONS") ;

  (Misc.make_method_name "isBacktraceRequested",
   Misc.make_field_name "BACKTRACE_REQUESTED") ;
]

let context_methods = List.map fst context_map

let fail_if_void : Descriptor.java_type -> Descriptor.for_field =
  function
    | `Void    -> assert false
    | `Array x -> `Array x
    | `Boolean -> `Boolean
    | `Byte    -> `Byte
    | `Char    -> `Char
    | `Class x -> `Class x
    | `Double  -> `Double
    | `Float   -> `Float
    | `Int     -> `Int
    | `Long    -> `Long
    | `Short   -> `Short

let access_context_through_fields l =
  let rec rewrite acc = function
    | (line, Instruction.INVOKESTATIC (cn, mn, ([], ret))) :: tl
      when (Name.equal_for_class Names.current_context cn)
          && List.exists (Name.equal_for_method mn) context_methods ->
        let fn =
          List.find
            (fun (x, _) -> Name.equal_for_method mn x)
            context_map
          |> snd in
        let acc =
          (line,
           Instruction.GETSTATIC (cn, fn, fail_if_void ret)) :: acc in
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let container_classes_map = [
  (Misc.make_class_name_ext "org.ocamljava.runtime.values.BasicBlockValue",
   Misc.make_class_name_ext "org.ocamljava.runtime.values.UnsafeBasicBlockValue") ;

  (Misc.make_class_name_ext "org.ocamljava.runtime.values.DoubleArrayValue",
   Misc.make_class_name_ext "org.ocamljava.runtime.values.UnsafeDoubleArrayValue") ;

  (Misc.make_class_name_ext "org.ocamljava.runtime.values.LongBlockValue",
   Misc.make_class_name_ext "org.ocamljava.runtime.values.UnsafeLongBlockValue") ;
]

let safe_container_classes =
  List.map fst container_classes_map

let unsafe cn =
  container_classes_map
  |> List.find (fun (s, _) -> Name.equal_for_class cn s)
  |> snd

let is_safe_class cn =
  safe_container_classes
  |> List.exists (fun x -> Name.equal_for_class cn x)

let safe_to_unsafe l =
  let rec rewrite acc = function
    | (line, Instruction.NEW cn) :: tl
      when (is_safe_class cn) ->
        let acc = (line, Instruction.NEW (unsafe cn)) :: acc in
        rewrite acc tl
    | (line, Instruction.INVOKESPECIAL (cn, mn, md)) :: tl
      when (Name.equal_for_method mn Names.cstr_name)
          && (is_safe_class cn) ->
        let acc =
          (line,
           Instruction.INVOKESPECIAL (unsafe cn, mn, md)) :: acc in
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let constant_loading l =
  let rec rewrite acc = function
    | (_, (Instruction.LDC_W (`String _) | Instruction.LDC (`String _)))
      :: (_, Instruction.INVOKESTATIC (stat_cn, stat_mn, ([`Class stat_p1], `Class stat_rt)))
      :: (_, Instruction.PUTSTATIC (_, _, `Class put_ft)) :: tl
      when (Name.equal_for_class Names.thread_local_factory stat_cn)
          && (Name.equal_for_method Names.global_storage stat_mn)
          && (Name.equal_for_class Names.string stat_p1)
          && (Name.equal_for_class Names.thread_local stat_rt)
          && (Name.equal_for_class Names.thread_local put_ft) ->
        rewrite acc tl
    | (line, ((Instruction.LDC_W (`Class_or_interface class_name))
            | (Instruction.LDC (`Class_or_interface class_name))))
      :: (_, Instruction.INVOKESTATIC (stat_cn, stat_mn, ([`Class stat_p1], `Class stat_rt)))
      :: tl
      when (Name.equal_for_class Names.thread_local_factory stat_cn)
          && (Name.equal_for_method Names.constants_storage stat_mn)
          && (Name.equal_for_class Names.class_ stat_p1)
          && (Name.equal_for_class Names.thread_local stat_rt) ->
        let call = Instruction.INVOKESTATIC (class_name,
                                             Names.create_constants,
                                             ([], `Void)) in
        rewrite ((line, call) :: acc) tl
    | (line, Instruction.PUTSTATIC (cn, fn, `Class ft)) :: tl
      when (Name.equal_for_field Names.globals fn)
          && (Name.equal_for_class Names.thread_local ft) ->
        let access =
          Instruction.PUTSTATIC
            (cn, fn, `Class (Misc.global_class_of_class cn)) in
        rewrite ((line, access) :: acc) tl
    | (_, Instruction.PUTSTATIC (_, fn, `Class ft)) :: tl
      when (Name.equal_for_field Names.constants fn)
          && (Name.equal_for_class Names.thread_local ft) ->
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let is_astore = function
  | Instruction.ASTORE_0
  | Instruction.ASTORE_1
  | Instruction.ASTORE_2
  | Instruction.ASTORE_3
  | Instruction.ASTORE _
  | Instruction.WIDE_ASTORE _ -> true
  | _ -> false

let extract_astore_index = function
  | Instruction.ASTORE_0 -> Some 0
  | Instruction.ASTORE_1 -> Some 1
  | Instruction.ASTORE_2 -> Some 2
  | Instruction.ASTORE_3 -> Some 3
  | Instruction.ASTORE i -> Some (i :> int)
  | Instruction.WIDE_ASTORE i -> Some (i :> int)
  | _ -> None

let is_aload_with_index index instr =
  match index, instr with
  | Some 0, Instruction.ALOAD_0 -> true
  | Some 1, Instruction.ALOAD_1 -> true
  | Some 2, Instruction.ALOAD_2 -> true
  | Some 3, Instruction.ALOAD_3 -> true
  | Some i1, Instruction.ALOAD i2 -> i1 = (i2 :> int)
  | Some i1, Instruction.WIDE_ALOAD i2 -> i1 = (i2 :> int)
  | _ -> false

let accesses_to_globals_and_constants l =
  let rec rewrite index acc = function
    | (_, Instruction.GETSTATIC (_, stat_fn, `Class stat_ft))
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface virt_cn, virt_mn, ([], `Class virt_rt)))
      :: (_, Instruction.CHECKCAST (`Class_or_interface _))
      :: (_, astore_instr)
      :: tl
      when (Name.equal_for_field Names.constants stat_fn)
          && (Name.equal_for_class Names.thread_local stat_ft)
          && (Name.equal_for_class Names.thread_local virt_cn)
          && (Name.equal_for_method Names.get virt_mn)
          && (Name.equal_for_class Names.object_ virt_rt)
          && (is_astore astore_instr) ->
        rewrite (extract_astore_index astore_instr) acc tl
    | (_, Instruction.GETSTATIC (_, stat_fn, `Class stat_ft))
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface virt_cn, virt_mn, ([], `Class virt_rt)))
      :: (_, Instruction.CHECKCAST (`Class_or_interface _))
      :: tl
      when (Name.equal_for_field Names.constants stat_fn)
          && (Name.equal_for_class Names.thread_local stat_ft)
          && (Name.equal_for_class Names.thread_local virt_cn)
          && (Name.equal_for_method Names.get virt_mn)
          && (Name.equal_for_class Names.object_ virt_rt) ->
        rewrite index acc tl
    | (line, aload_instr)
      :: (_, Instruction.GETFIELD (cn, fn, fd))
      :: tl
      when (WholeProgram.is_constants_class cn)
          && (is_aload_with_index index aload_instr) ->
        let cn = WholeProgram.main_class_of_constants_class cn in
        let acc =
          (line, Instruction.GETSTATIC (cn, fn, fd))
          :: acc in
        rewrite index acc tl
    | (line, Instruction.GETFIELD (cn, fn, fd))
      :: tl
      when WholeProgram.is_constants_class cn ->
        let cn = WholeProgram.main_class_of_constants_class cn in
        let acc = (line, Instruction.GETSTATIC (cn, fn, fd)) :: acc in
        rewrite index acc tl
    | (line, Instruction.GETSTATIC (stat_cn, stat_fn, `Class stat_ft))
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface virt_cn, virt_mn, ([], `Class virt_rt)))
      :: (_, Instruction.CHECKCAST (`Class_or_interface _)) :: tl
      when (Name.equal_for_field Names.globals stat_fn)
          && (Name.equal_for_class Names.thread_local stat_ft)
          && (Name.equal_for_class Names.thread_local virt_cn)
          && (Name.equal_for_method Names.get virt_mn)
          && (Name.equal_for_class Names.object_ virt_rt) ->
        let acc = (line, Instruction.GETSTATIC (stat_cn, stat_fn, `Class (Misc.global_class_of_class stat_cn))) :: acc in
        rewrite index acc tl
    | ((_, Instruction.NEW cn) as instr_new)
      :: ((_, Instruction.DUP) as instr_dup)
      :: ((_, Instruction.INVOKESPECIAL _) as instr_invoke)
      :: ((line, not_nop) as instr_next)
      :: tl
      when (not_nop <> Instruction.NOP)
          && (Name.equal_for_class cn (Misc.global_class_of_class !State.current_class_name)) ->
        let acc =
          instr_next
          :: (line, Instruction.PUTSTATIC
                (!State.current_class_name,
                 Names.globals,
                 `Class (Misc.global_class_of_class !State.current_class_name)))
          :: (line, Instruction.DUP)
          :: (line, Instruction.NOP) (* to ensure termination of rewriting rules *)
          :: instr_invoke
          :: instr_dup
          :: instr_new
          :: acc in
        rewrite index acc tl
    | hd :: tl -> rewrite index (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite None [] l

let is_aload = function
  | Instruction.ALOAD_0
  | Instruction.ALOAD_1
  | Instruction.ALOAD_2
  | Instruction.ALOAD_3
  | Instruction.ALOAD _
  | Instruction.WIDE_ALOAD _ -> true
  | _ -> false

let is_ldc_class_name = function
  | Instruction.LDC (`Class_or_interface _)
  | Instruction.LDC_W (`Class_or_interface _) -> true
  | _ -> false

let remove_load_constant l =
  let rec rewrite acc = function
    | (_, instr1)
      :: (_, instr2)
      :: (_, Instruction.INVOKESTATIC (_, stat_mn, ([], _)))
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface virt_cn, virt_mn, ([`Class virt_p1; `Class _], `Void)))
      :: tl
      when (is_aload instr1)
          && (is_ldc_class_name instr2)
          && (Name.equal_for_method Names.create_constants stat_mn)
          && (Name.equal_for_class Names.abstract_native_runner virt_cn)
          && (Name.equal_for_method Names.set_constant virt_mn)
          && (Name.equal_for_class Names.class_ virt_p1) ->
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let simplify_calls_to_entry l =
  let rec rewrite acc = function
    | (_, Instruction.ALOAD_0)
      :: (_, Instruction.DUP)
      :: ((line, Instruction.INVOKESTATIC (_, stat_mn, ([], (`Class stat_rt)))) as call)
      :: (_, Instruction.PUTFIELD (put_cn, put_fn, `Class put_ft))
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface virt_cn, virt_mn, ([], `Void)))
      :: tl
      when (Name.equal_for_method Names.entry stat_mn)
          && (Name.equal_for_class Names.value stat_rt)
          && (Name.equal_for_class Names.abstract_native_runner put_cn)
          && (Name.equal_for_field Names.result put_fn)
          && (Name.equal_for_class Names.value put_ft)
          && (Name.equal_for_class Names.abstract_native_runner virt_cn)
          && (Name.equal_for_method Names.incr_globals_inited virt_mn) ->
        let acc = (line, Instruction.POP) :: call :: acc in
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let create_constants l =
  let rec rewrite acc = function
    | (_, (Instruction.ALOAD_0 | Instruction.ASTORE_0))  :: tl ->
        rewrite acc tl
    | (line, Instruction.ARETURN) :: tl ->
        let acc = (line, Instruction.RETURN) :: acc in
        rewrite acc tl
    | (line, Instruction.PUTFIELD (cn, fn, fd)) :: tl
      when WholeProgram.is_constants_class cn ->
        let cn = WholeProgram.main_class_of_constants_class cn in
        let acc = (line, Instruction.PUTSTATIC (cn, fn, fd)) :: acc in
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  match l with
  | (_, Instruction.NEW _)
    :: (_, Instruction.DUP)
    :: (_, Instruction.INVOKESPECIAL _)
    :: (_, (Instruction.ASTORE_0 | Instruction.DUP))
    :: l -> rewrite [] l
  | _ -> rewrite [] l

let remove_shared_constants l =
  (* note: it is OK to use peephole rule to remove shared constants,
     because the code cannot contain jumps *)
  let rec rewrite keep acc = function
    | (_, Instruction.INVOKESTATIC (cn, mn, ([], `Void)))
      :: tl
      when (Name.equal_for_class cn Names.abstract_native_runner)
          && (Name.equal_for_method mn Names.shared_constants_begin) ->
        rewrite false acc tl
    | (_, Instruction.INVOKESTATIC (cn, mn, ([], `Void)))
      :: tl
      when (Name.equal_for_class cn Names.abstract_native_runner)
          && (Name.equal_for_method mn Names.shared_constants_end) ->
        rewrite true acc tl
    | hd :: tl ->
        if keep then
          rewrite keep (hd :: acc) tl
        else
          rewrite keep acc tl
    | [] -> List.rev acc in
  rewrite true [] l

let use_shared_constants l =
  let rec rewrite acc = function
    | (line, Instruction.GETSTATIC (_, fn, `Class cn))
      :: tl
      when (Name.equal_for_class cn Names.value) && (Misc.is_shared_constant fn) ->
        let acc =
          (line,
           Instruction.GETSTATIC
             (!WholeProgram.shared_constants_class_name,
              fn,
              `Class cn)) :: acc in
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let remove_unused_globals l =
  let rec rewrite acc = function
    | (_, Instruction.GETSTATIC (cn, fn, `Class fd))
      :: (_, Instruction.POP)
      :: tl
      when (Name.equal_for_class cn Names.value)
          && (Name.equal_for_field fn Names.unit)
          && (Name.equal_for_class fd Names.value) ->
        rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let remove_nops l =
  let rec rewrite acc = function
    | (_, Instruction.NOP) :: tl -> rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l
