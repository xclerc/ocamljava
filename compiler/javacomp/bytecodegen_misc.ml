(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
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

open Bytecodeutils
open Jlambda
open Macroinstr
open BaristaLibrary


module State = struct

  let curr_file = ref ""

  let curr_class = ref ""

  let curr_function = ref ""

  let curr_module = ref ""

  let table = ref []

  let line_number_table = ref []

  let prev_ofs = ref 0

  let current_catches : (int, int ref) Hashtbl.t = Hashtbl.create 17

  let tail_calls = ref 0

  let non_tail_calls = ref 0

  let reset_state ~file ~clas ~func =
    curr_file := file;
    curr_class := clas;
    curr_function := func;
    curr_module := Jcompilenv.make_symbol None;
    table := [];
    line_number_table := [];
    prev_ofs := 0;
    Hashtbl.clear current_catches;
    tail_calls := 0;
    non_tail_calls := 0

  let current_file () =
    !curr_file

  let current_class () =
    !curr_class

  let current_function () =
    !curr_function

  let current_module () =
    !curr_module

  let is_same_function cls func =
    (cls = !curr_class) && (func = !curr_function)

  let add_exception start_pc end_pc handler_pc =
    table := (start_pc, end_pc, handler_pc) :: !table

  let compile_exception_table () =
    List.rev_map
      (fun ((start_pc : Utils.u2), (end_pc : Utils.u2), (handler_pc : Utils.u2)) ->
        { Attribute.try_start = start_pc;
          Attribute.try_end = end_pc;
          Attribute.catch = handler_pc;
          Attribute.caught = None; })
      !table

  let add_debug_info ofs dbg =
    if dbg != Debuginfo.none && dbg.Debuginfo.dinfo_file = !curr_file then begin
      line_number_table := (Utils.u2 !prev_ofs,
                            Utils.u2 dbg.Debuginfo.dinfo_line) :: !line_number_table;
      prev_ofs := ofs
    end

  let compile_line_numbers () =
    !line_number_table

  let add_catch id offset =
    Hashtbl.add current_catches id offset

  let get_catch_offset id =
    Hashtbl.find current_catches id

  let incr_tail_calls () =
    incr tail_calls

  let incr_non_tail_calls () =
    incr non_tail_calls

  let compile_method_infos () =
    class_MethodInfos,
    [ UTF8.of_string "tailCalls", Annotation.Int_value (Int32.of_int !tail_calls) ;
      UTF8.of_string "nonTailCalls", Annotation.Int_value (Int32.of_int !non_tail_calls) ]

end

let repeat_parameters n =
  let rec params acc = function
    | 0 -> acc
    | n -> params ((`Class class_Value) :: acc) (pred n) in
  params [] n

let repeat_doubles n =
  let rec dbls acc = function
    | 0 -> acc
    | n -> dbls (`Double :: acc) (pred n) in
  dbls [] n

let kload kind idx =
  match kind with
  | Macroinstr.Boxed_value          -> aload idx
  | Macroinstr.Tagged_int           -> lload idx
  | Macroinstr.Normalized_int       -> lload idx
  | Macroinstr.Unboxed_int          -> lload idx
  | Macroinstr.Unboxed_int32        -> iload idx
  | Macroinstr.Unboxed_int64        -> lload idx
  | Macroinstr.Unboxed_nativeint    -> lload idx
  | Macroinstr.Unboxed_float        -> dload idx
  | Macroinstr.Unboxed_instance _   -> aload idx
  | Macroinstr.Unboxed_java_array _ -> aload idx

let kstore kind idx =
  match kind with
  | Macroinstr.Boxed_value          -> astore idx
  | Macroinstr.Tagged_int           -> lstore idx
  | Macroinstr.Normalized_int       -> lstore idx
  | Macroinstr.Unboxed_int          -> lstore idx
  | Macroinstr.Unboxed_int32        -> istore idx
  | Macroinstr.Unboxed_int64        -> lstore idx
  | Macroinstr.Unboxed_nativeint    -> lstore idx
  | Macroinstr.Unboxed_float        -> dstore idx
  | Macroinstr.Unboxed_instance _   -> astore idx
  | Macroinstr.Unboxed_java_array _ -> astore idx

let node x = Instrtree.node x

let leaf x = Instrtree.leaf x

let string_of_value_kind = function
  | Boxed_value            -> "Boxed_value"
  | Tagged_int             -> "Tagged_int"
  | Normalized_int         -> "Normalized_int"
  | Unboxed_int            -> "Unboxed_int"
  | Unboxed_int32          -> "Unboxed_int32"
  | Unboxed_int64          -> "Unboxed_int64"
  | Unboxed_nativeint      -> "Unboxed_nativeint"
  | Unboxed_float          -> "Unboxed_float"
  | Unboxed_instance cn    -> "Unboxed_instance " ^ cn
  | Unboxed_java_array arr ->
      let arr =
        Descriptor.external_utf8_of_java_type
          (unconvert_array_type arr :> Descriptor.java_type) in
      "Unboxed_java_array " ^ (UTF8.to_string arr)

let compile_conversion ?(relaxed_mode = false) src dst =
  let nop = leaf [] in
  let impossible dummy =
    if relaxed_mode then begin
      leaf dummy
    end else begin
      Printf.sprintf "Bytecodegen_misc.compile_conversion: %s, %s"
        (string_of_value_kind src)
        (string_of_value_kind dst)
      |> Misc.fatal_error
    end in
  let untag l =
    leaf ([ Instruction.ICONST_1 ; Instruction.LSHR ] @ l ) in
  let tag l =
    leaf (l @ [ Instruction.ICONST_1 ; Instruction.LSHL ;
                Instruction.LCONST_1 ; Instruction.LOR ]) in
  let dummy_reference simple =
    [ if simple then Instruction.POP else Instruction.POP2 ;
      Instruction.ACONST_NULL ] in
  let dummy_long tagged =
    [ Instruction.POP ;
      if tagged then Instruction.LCONST_1 else Instruction.LCONST_0 ] in
  let dummy_int32 =
    [ Instruction.POP ; Instruction.ICONST_0 ] in
  let dummy_float =
    [ Instruction.POP ; Instruction.DCONST_0 ] in
  match src, dst with
  | Boxed_value, Boxed_value -> nop
  | Boxed_value, Tagged_int -> meth_getRawValue
  | Boxed_value, Normalized_int -> meth_asLong
  | Boxed_value, Unboxed_int -> meth_asLong
  | Boxed_value, Unboxed_int32 -> meth_asInt32
  | Boxed_value, Unboxed_int64 -> meth_asInt64
  | Boxed_value, Unboxed_nativeint -> meth_asNativeInt
  | Boxed_value, Unboxed_float -> meth_asDouble
  | Boxed_value, Unboxed_instance cn ->
      if cn = "java.lang.Object" then
        node [ meth_asCustom ]
      else
        node [ meth_asCustom ; leaf [ Instruction.CHECKCAST (`Class_or_interface (make_class cn)) ] ]
  | Boxed_value, Unboxed_java_array arr -> node [ meth_asCustom ; leaf [ Instruction.CHECKCAST (`Array_type (unconvert_array_type arr)) ] ]

  | Tagged_int, Boxed_value -> meth_createFromRawLong
  | Tagged_int, Tagged_int -> nop
  | Tagged_int, Normalized_int -> untag []
  | Tagged_int, Unboxed_int -> untag []
  | Tagged_int, Unboxed_int32 -> untag [ Instruction.L2I ]
  | Tagged_int, Unboxed_int64 -> untag []
  | Tagged_int, Unboxed_nativeint -> untag []
  | Tagged_int, Unboxed_float -> untag [ Instruction.L2D ]
  | Tagged_int, Unboxed_instance _ -> impossible @@ dummy_reference false
  | Tagged_int, Unboxed_java_array _ -> impossible @@ dummy_reference false

  | Normalized_int, Boxed_value -> meth_createLong
  | Normalized_int, Tagged_int -> tag []
  | Normalized_int, Normalized_int -> nop
  | Normalized_int, Unboxed_int -> nop
  | Normalized_int, Unboxed_int32 -> leaf [ Instruction.L2I ]
  | Normalized_int, Unboxed_int64 -> nop
  | Normalized_int, Unboxed_nativeint -> nop
  | Normalized_int, Unboxed_float -> leaf [ Instruction.L2D ]
  | Normalized_int, Unboxed_instance _ -> impossible @@ dummy_reference false
  | Normalized_int, Unboxed_java_array _ -> impossible @@ dummy_reference false

  | Unboxed_int, Boxed_value -> meth_createLong
  | Unboxed_int, Tagged_int -> tag []
  | Unboxed_int, Normalized_int -> nop
  | Unboxed_int, Unboxed_int -> nop
  | Unboxed_int, Unboxed_int32 -> leaf [ Instruction.L2I ]
  | Unboxed_int, Unboxed_int64 -> nop
  | Unboxed_int, Unboxed_nativeint -> nop
  | Unboxed_int, Unboxed_float -> leaf [ Instruction.L2D ]
  | Unboxed_int, Unboxed_instance _ -> impossible @@ dummy_reference false
  | Unboxed_int, Unboxed_java_array _ -> impossible @@ dummy_reference false

  | Unboxed_int32, Boxed_value -> meth_createInt32
  | Unboxed_int32, Tagged_int -> tag [ Instruction.I2L ]
  | Unboxed_int32, Normalized_int -> leaf [ Instruction.I2L ]
  | Unboxed_int32, Unboxed_int -> leaf [ Instruction.I2L ]
  | Unboxed_int32, Unboxed_int32 -> nop
  | Unboxed_int32, Unboxed_int64 -> leaf [ Instruction.I2L ]
  | Unboxed_int32, Unboxed_nativeint -> leaf [ Instruction.I2L ]
  | Unboxed_int32, Unboxed_float -> leaf [ Instruction.I2D ]
  | Unboxed_int32, Unboxed_instance _ -> impossible @@ dummy_reference true
  | Unboxed_int32, Unboxed_java_array _ -> impossible @@ dummy_reference true

  | Unboxed_int64, Boxed_value -> meth_createInt64
  | Unboxed_int64, Tagged_int -> tag []
  | Unboxed_int64, Normalized_int -> nop
  | Unboxed_int64, Unboxed_int -> nop
  | Unboxed_int64, Unboxed_int32 -> leaf [ Instruction.L2I ]
  | Unboxed_int64, Unboxed_int64 -> nop
  | Unboxed_int64, Unboxed_nativeint -> nop
  | Unboxed_int64, Unboxed_float -> leaf [ Instruction.L2D ]
  | Unboxed_int64, Unboxed_instance _ -> impossible @@ dummy_reference false
  | Unboxed_int64, Unboxed_java_array _ -> impossible @@ dummy_reference false

  | Unboxed_nativeint, Boxed_value -> meth_createNativeInt
  | Unboxed_nativeint, Tagged_int -> tag []
  | Unboxed_nativeint, Normalized_int -> nop
  | Unboxed_nativeint, Unboxed_int -> nop
  | Unboxed_nativeint, Unboxed_int32 -> leaf [ Instruction.L2I ]
  | Unboxed_nativeint, Unboxed_int64 -> nop
  | Unboxed_nativeint, Unboxed_nativeint -> nop
  | Unboxed_nativeint, Unboxed_float -> leaf [ Instruction.L2D ]
  | Unboxed_nativeint, Unboxed_instance _ -> impossible @@ dummy_reference false
  | Unboxed_nativeint, Unboxed_java_array _ -> impossible @@ dummy_reference false

  | Unboxed_float, Boxed_value -> meth_createDouble
  | Unboxed_float, Tagged_int -> tag [ Instruction.D2L ]
  | Unboxed_float, Normalized_int -> leaf [ Instruction.D2L ]
  | Unboxed_float, Unboxed_int -> leaf [ Instruction.D2L ]
  | Unboxed_float, Unboxed_int32 -> leaf [ Instruction.D2I ]
  | Unboxed_float, Unboxed_int64 -> leaf [ Instruction.D2L ]
  | Unboxed_float, Unboxed_nativeint -> leaf [ Instruction.D2L ]
  | Unboxed_float, Unboxed_float -> nop
  | Unboxed_float, Unboxed_instance _ -> impossible @@ dummy_reference false
  | Unboxed_float, Unboxed_java_array _ -> impossible @@ dummy_reference false

  | Unboxed_instance _, Boxed_value -> meth_createInstance
  | Unboxed_instance _, Tagged_int -> impossible @@ dummy_long true
  | Unboxed_instance _, Normalized_int -> impossible @@ dummy_long false
  | Unboxed_instance _, Unboxed_int -> impossible @@ dummy_long false
  | Unboxed_instance _, Unboxed_int32 -> impossible dummy_int32
  | Unboxed_instance _, Unboxed_int64 -> impossible @@ dummy_long false
  | Unboxed_instance _, Unboxed_nativeint -> impossible @@ dummy_long false
  | Unboxed_instance _, Unboxed_float -> impossible dummy_float
  | Unboxed_instance cn_src, Unboxed_instance cn_dst ->
      if cn_dst = "java.lang.Object" then
        nop
      else begin
        let cn_src = make_class cn_src in
        let cn_dst = make_class cn_dst in
        if Jutils.is_subtype cn_src cn_dst then
          nop
        else
          node [ leaf [ Instruction.CHECKCAST (`Class_or_interface cn_dst) ] ]
      end
  | Unboxed_instance _, Unboxed_java_array arr -> leaf [ Instruction.CHECKCAST (`Array_type (unconvert_array_type arr)) ]

  | Unboxed_java_array _, Boxed_value -> meth_createInstance
  | Unboxed_java_array _, Tagged_int -> impossible @@ dummy_long true
  | Unboxed_java_array _, Normalized_int -> impossible @@ dummy_long false
  | Unboxed_java_array _, Unboxed_int -> impossible @@ dummy_long false
  | Unboxed_java_array _, Unboxed_int32 -> impossible dummy_int32
  | Unboxed_java_array _, Unboxed_int64 -> impossible @@ dummy_long false
  | Unboxed_java_array _, Unboxed_nativeint -> impossible @@ dummy_long false
  | Unboxed_java_array _, Unboxed_float -> impossible dummy_float
  | Unboxed_java_array _, Unboxed_instance cn ->
      if cn = "java.lang.Object" then
        leaf [ Instruction.CHECKCAST (`Class_or_interface class_Object) ]
      else
        impossible []
  | Unboxed_java_array _, Unboxed_java_array arr -> leaf [ Instruction.CHECKCAST (`Array_type (unconvert_array_type arr)) ]

type error =
  | Method_too_long of string * bool * int
  | Unable_to_compute_stack of string * string
  | Unable_to_optimize of string * string
  | Invalid_external_declaration of string
  | Varargs_should_be_literal_array

exception Error of error

let report_error ppf = function
  | Method_too_long (name, total, bytes) ->
      if total then
        Format.fprintf ppf "Cannot compile %s (Java method is too long: %d bytes)"
          name
          bytes
      else
        Format.fprintf ppf "Cannot compile %s (jump offset of %d)"
          name
          bytes
  | Unable_to_compute_stack (name, msg) ->
      Format.fprintf ppf "Cannot compute stack frames for %s@ (%s)"
        name
        msg
  | Unable_to_optimize (name, msg) ->
      Format.fprintf ppf "Cannot optimize bytecode for %s @ (%s)"
        name
        msg
  | Invalid_external_declaration decl ->
      Format.fprintf ppf "Invalid external declaration@ (%s)"
        decl
  | Varargs_should_be_literal_array ->
      Format.fprintf ppf "Vararg values should be passed as a literal array"

let check_jump ofs =
  if ofs >= -32768 && ofs <= 32767 then
    Utils.s2 ofs
  else
    raise (Error (Method_too_long (State.current_function (), false, ofs)))
