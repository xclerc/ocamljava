(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
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
open Bytecodeutils
open Bytecodegen_misc


let get_store_load = function
  | 0 -> Instruction.ASTORE_0, Instruction.ALOAD_0
  | 1 -> Instruction.ASTORE_1, Instruction.ALOAD_1
  | 2 -> Instruction.ASTORE_2, Instruction.ALOAD_2
  | 3 -> Instruction.ASTORE_3, Instruction.ALOAD_3
  | n when n >= 0 && n <= 255 ->
      Instruction.ASTORE (Utils.u1 n), Instruction.ALOAD (Utils.u1 n)
  | n when n >= 0 && n <= 65535 ->
      Instruction.WIDE_ASTORE (Utils.u2 n), Instruction.WIDE_ALOAD (Utils.u2 n)
  | _ ->
      Misc.fatal_error "Bytecodegen.astore/aload: invalid index"

let rewrite_constant set_zero get_zero get_constant l =
  let rec rewrite acc = function
    | (line, Instruction.ALOAD_0) :: tl ->
        let acc = (line, get_zero) :: acc in
        rewrite acc tl
    | (line, Instruction.ASTORE_0) :: tl ->
        let acc = (line, set_zero) :: acc in
        rewrite acc tl
    | (line, Instruction.GETSTATIC (cn1, fn1, `Class fd1))
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn2, mn2, ([], `Class md2)))
      :: (_, Instruction.CHECKCAST (`Class_or_interface _))
      :: tl
      when (Name.equal_for_class cn1 (make_class (State.current_class ())))
          && (Name.equal_for_field fn1 (make_field "CONSTANTS"))
          && (Name.equal_for_class fd1 class_ThreadLocal)
          && (Name.equal_for_class cn2 class_ThreadLocal)
          && (Name.equal_for_method mn2 (make_method "get"))
          && (Name.equal_for_class md2 class_Object) ->
            rewrite ((line, get_constant) :: acc) tl
    | ((line, (Instruction.BIPUSH _ | Instruction.SIPUSH _)) as push_instr)
      :: (_, Instruction.I2L)
      :: ((_, Instruction.ALOAD _) as aload_instr)
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn, mn, ([`Long; `Class cn2], `Void)))
      :: tl
      when (Name.equal_for_class cn class_Value)
          && (Name.equal_for_method mn (make_method "set"))
          && (Name.equal_for_class cn2 class_Value) ->
            let call = Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                                  make_method "set",
                                                  ([`Int; `Class class_Value], `Void)) in
            let acc =
              (line, call)
              :: aload_instr
              :: push_instr
              :: acc in
            rewrite acc tl
    | ((line, (Instruction.BIPUSH _ | Instruction.SIPUSH _)) as push_instr)
      :: (_, Instruction.I2L)
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn, mn, ([`Long], `Class cn2)))
      :: tl
      when (Name.equal_for_class cn class_Value)
          && (Name.equal_for_method mn (make_method "get"))
          && (Name.equal_for_class cn2 class_Value) ->
            let call = Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                                  make_method "get",
                                                  ([`Int], `Class class_Value)) in
            let acc =
              (line, call)
              :: push_instr
              :: acc in
            rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let shrink instrs =
  let max_locals =
    List.fold_left
      (fun acc i ->
        try
          let index =
            match i with
            | Instruction.ALOAD p       -> Utils.u2_of_u1 p
            | Instruction.ALOAD_0       -> Utils.u2 0
            | Instruction.ALOAD_1       -> Utils.u2 1
            | Instruction.ALOAD_2       -> Utils.u2 2
            | Instruction.ALOAD_3       -> Utils.u2 3
            | Instruction.ASTORE p      -> Utils.u2_of_u1 p
            | Instruction.ASTORE_0      -> Utils.u2 0
            | Instruction.ASTORE_1      -> Utils.u2 1
            | Instruction.ASTORE_2      -> Utils.u2 2
            | Instruction.ASTORE_3      -> Utils.u2 3
            | Instruction.DLOAD p       -> Utils.u2_succ (Utils.u2_of_u1 p)
            | Instruction.DLOAD_0       -> Utils.u2 1
            | Instruction.DLOAD_1       -> Utils.u2 2
            | Instruction.DLOAD_2       -> Utils.u2 3
            | Instruction.DLOAD_3       -> Utils.u2 4
            | Instruction.DSTORE p      -> Utils.u2_succ (Utils.u2_of_u1 p)
            | Instruction.DSTORE_0      -> Utils.u2 1
            | Instruction.DSTORE_1      -> Utils.u2 2
            | Instruction.DSTORE_2      -> Utils.u2 3
            | Instruction.DSTORE_3      -> Utils.u2 4
            | Instruction.FLOAD p       -> Utils.u2_of_u1 p
            | Instruction.FLOAD_0       -> Utils.u2 0
            | Instruction.FLOAD_1       -> Utils.u2 1
            | Instruction.FLOAD_2       -> Utils.u2 2
            | Instruction.FLOAD_3       -> Utils.u2 3
            | Instruction.FSTORE p      -> Utils.u2_of_u1 p
            | Instruction.FSTORE_0      -> Utils.u2 0
            | Instruction.FSTORE_1      -> Utils.u2 1
            | Instruction.FSTORE_2      -> Utils.u2 2
            | Instruction.FSTORE_3      -> Utils.u2 3
            | Instruction.ILOAD p       -> Utils.u2_of_u1 p
            | Instruction.ILOAD_0       -> Utils.u2 0
            | Instruction.ILOAD_1       -> Utils.u2 1
            | Instruction.ILOAD_2       -> Utils.u2 2
            | Instruction.ILOAD_3       -> Utils.u2 3
            | Instruction.ISTORE p      -> Utils.u2_of_u1 p
            | Instruction.ISTORE_0      -> Utils.u2 0
            | Instruction.ISTORE_1      -> Utils.u2 1
            | Instruction.ISTORE_2      -> Utils.u2 2
            | Instruction.ISTORE_3      -> Utils.u2 3
            | Instruction.LLOAD p       -> Utils.u2_succ (Utils.u2_of_u1 p)
            | Instruction.LLOAD_0       -> Utils.u2 1
            | Instruction.LLOAD_1       -> Utils.u2 2
            | Instruction.LLOAD_2       -> Utils.u2 3
            | Instruction.LLOAD_3       -> Utils.u2 4
            | Instruction.LSTORE p      -> Utils.u2_succ (Utils.u2_of_u1 p)
            | Instruction.LSTORE_0      -> Utils.u2 1
            | Instruction.LSTORE_1      -> Utils.u2 2
            | Instruction.LSTORE_2      -> Utils.u2 3
            | Instruction.LSTORE_3      -> Utils.u2 4
            | Instruction.WIDE_ALOAD p  -> p
            | Instruction.WIDE_ASTORE p -> p
            | Instruction.WIDE_DLOAD p  -> Utils.u2_succ p
            | Instruction.WIDE_DSTORE p -> Utils.u2_succ p
            | Instruction.WIDE_FLOAD p  -> p
            | Instruction.WIDE_FSTORE p -> p
            | Instruction.WIDE_ILOAD p  -> p
            | Instruction.WIDE_ISTORE p -> p
            | Instruction.WIDE_LLOAD p  -> Utils.u2_succ p
            | Instruction.WIDE_LSTORE p -> Utils.u2_succ p
            | _ -> raise Not_found in
          Utils.max_u2 (Utils.u2_succ index) acc
        with Not_found -> acc)
      (Utils.u2 0)
      instrs in
  let set_zero, get_zero = get_store_load (succ (max_locals :> int)) in
  let set_constant, get_constant = get_store_load (2 + (max_locals :> int)) in
  let rewrite_constant = rewrite_constant set_zero get_zero get_constant in
  let instrs =
    instrs
    |> List.map (fun x -> Utils.u2 0, x)
    |> Peephole.optimize_list ~rules:(rewrite_constant :: peephole_rules)
    |> List.map
        (fun (_, i) ->
          match i with
          | Instruction.GETSTATIC (cn, fn, _)
            when (Name.equal_for_class cn class_Value)
                && ((UTF8.to_string (Name.utf8_for_field fn)) = "ZERO") ->
                  Instruction.ALOAD_0
          | _ -> i) in
  let const_class =
    Bytecodegen_constants.const_class_of_curr_class (State.current_class ()) in
  Instruction.ALOAD_0
  :: set_zero
  :: (Instruction.GETSTATIC (class_Value,
                             Name.make_for_field (UTF8.of_string "ZERO"),
                             `Class class_Value))
  :: Instruction.ASTORE_0
  :: (Instruction.GETSTATIC (make_class (State.current_class ()),
                             make_field "CONSTANTS",
                             `Class class_ThreadLocal))
  :: (Instruction.INVOKEVIRTUAL (`Class_or_interface class_ThreadLocal,
                                 make_method "get",
                                 ([], `Class class_Object)))
  :: (Instruction.CHECKCAST (`Class_or_interface const_class))
  :: set_constant
  :: instrs
