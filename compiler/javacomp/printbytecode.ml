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

open Format
open BaristaLibrary

let type_descriptor ppf (t : Descriptor.java_type) =
  t
  |> Descriptor.external_utf8_of_java_type
  |> UTF8.to_string
  |> pp_print_string ppf

let type_descriptor_list ppf (l : Descriptor.for_parameter list) =
  match l with
  | hd :: tl ->
      type_descriptor ppf (hd :> Descriptor.java_type);
      List.iter
        (fun t ->
          fprintf ppf ",@ %a"
            type_descriptor (t :> Descriptor.java_type))
        tl
  | [] ->
      ()

let class_name ppf cn =
  cn
  |> Name.external_utf8_for_class
  |> UTF8.to_string
  |> pp_print_string ppf

let field_name ppf fn =
  fn
  |> Name.utf8_for_field
  |> UTF8.to_string
  |> pp_print_string ppf

let method_name ppf mn =
  mn
  |> Name.utf8_for_method
  |> UTF8.to_string
  |> pp_print_string ppf

let method_handle ppf mh =
  let reference_ppf ppf (cn, fn, fd) =
    fprintf ppf "%a.%a:%a"
      class_name cn
      field_name fn
      type_descriptor (fd :> Descriptor.java_type) in
  let method_ppf ppf (cn, mn, (params, ret)) =
    fprintf ppf "%a.%a(%a):%a"
      class_name cn
      method_name mn
      type_descriptor_list params
      type_descriptor ret in
  let constructor_ppf ppf (cn, params) =
    fprintf ppf "%a(%a)"
      class_name cn
      type_descriptor_list params in
  match mh with
  | `getField f         -> fprintf ppf "getField %a" reference_ppf f
  | `getStatic f        -> fprintf ppf "getStatic %a" reference_ppf f
  | `invokeInterface m  -> fprintf ppf "invokeInterface %a" method_ppf m
  | `invokeSpecial m    -> fprintf ppf "invokeSpecial %a" method_ppf m
  | `invokeStatic m     -> fprintf ppf "invokeStatic %a" method_ppf m
  | `invokeVirtual m    -> fprintf ppf "invokeVirtual %a" method_ppf m
  | `newInvokeSpecial c -> fprintf ppf "newInvokeSpecial %a"constructor_ppf c
  | `putField f         -> fprintf ppf "putField %a" reference_ppf f
  | `putStatic f        -> fprintf ppf "putStatic %a" reference_ppf f

let utf8 ppf str =
  str
  |> UTF8.to_string
  |> pp_print_string ppf

let parameter offset ppf p =
  match p with
  | Instruction.Int_constant i ->
      fprintf ppf "%Ld" i
  | Instruction.Offset o ->
      fprintf ppf "%ld" Int32.(add o (of_int offset))
  | Instruction.Float_constant f ->
      fprintf ppf "%f" f
  | Instruction.String_constant s ->
      utf8 ppf (UTF8.escape s)
  | Instruction.Class_name cn ->
      class_name ppf cn
  | Instruction.Array_type at ->
      utf8 ppf at
  | Instruction.Primitive_type pt ->
      type_descriptor ppf pt
  | Instruction.Field (cn, fn, fd) ->
      fprintf ppf "%a.%a:%a"
        class_name cn
        field_name fn
        type_descriptor (fd :> Descriptor.java_type)
  | Instruction.Dynamic_method ((mh, _), mn, (params, ret)) ->
      fprintf ppf "[bootstrap: %a(...)] %a(%a):%a"
        method_handle mh
        method_name mn
        type_descriptor_list params
        type_descriptor ret
  | Instruction.Method (cn, mn, (params, ret)) ->
      fprintf ppf "%a.%a(%a):%a"
        class_name cn
        method_name mn
        type_descriptor_list params
        type_descriptor ret
  | Instruction.Array_method (at, mn, (params, ret)) ->
      fprintf ppf "%a.%a(%a):%a"
        type_descriptor (at :> Descriptor.java_type)
        method_name mn
        type_descriptor_list params
        type_descriptor ret
  | Instruction.Method_type_constant (params, ret) ->
      fprintf ppf "(%a):%a"
        type_descriptor_list params
        type_descriptor ret
  | Instruction.Method_handle_constant mh ->
      method_handle ppf mh

let parameters ppf offset l =
  List.iter
    (fun p ->
      fprintf ppf " %a" (parameter offset) p)
    l

let parameters_tail ppf offset = function
  | Instruction.No_tail ->
      pp_print_newline ppf ()
  | Instruction.Match_offset_pairs l ->
      pp_print_newline ppf ();
      List.iter
        (fun (mat, ofs) ->
          fprintf ppf "  %ld -> %ld\n"
            (mat : Utils.s4 :> int32)
            Int32.(add (of_int offset) (ofs : Instruction.long_offset :> t)))
        l
  | Instruction.Long_offsets l ->
      pp_print_newline ppf ();
      List.iter
        (fun ofs ->
          fprintf ppf "  %ld\n"
            Int32.(add (of_int offset) (ofs : Instruction.long_offset :> t)))
        l

let bytecode ppf header instrs exception_table =
  pp_print_string ppf header;
  pp_print_newline ppf ();
  let offset = ref 0 in
  List.iter
    (fun instr ->
      let sz, wide, mnemo, p, t = Instruction.decompile !offset instr in
      fprintf ppf "%05d %s%s"
        (!offset)
        (if wide then "wide " else "")
        mnemo;
      parameters ppf !offset p;
      parameters_tail ppf !offset t;
      offset := !offset + sz)
    instrs;
  List.iter
    (fun elem ->
      let caught =
        match elem.Attribute.caught with
        | Some n -> UTF8.to_string (Name.external_utf8_for_class n)
        | None -> "_" in
      fprintf ppf "handler at %d protecting from %d to %d for %s"
        (elem.Attribute.catch :> int)
        (elem.Attribute.try_start :> int)
        (elem.Attribute.try_end :> int)
        caught;
      pp_print_newline ppf ())
    exception_table;
  pp_print_newline ppf ();
  pp_print_flush ppf ()
