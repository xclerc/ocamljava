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

open Macroinstr
open Bytecodeutils
open BaristaLibrary
open Utils


let at_most n l =
  let rec am acc n l =
    if n <= 0 then
      List.rev acc
    else begin
      match l with
      | hd :: tl -> am ((Some hd) :: acc) (pred n) tl
      | [] -> am (None :: acc) (pred n) []
    end in
  am [] n l

let make_field_name x =
  make_field (Printf.sprintf "field_%d" x)

let make_code ~max_stack ~max_locals ?(attrs = []) l =
  Attribute.({ max_stack = u2 max_stack;
               max_locals = u2 max_locals;
               code = l;
               exception_table = [];
               attributes = attrs; })

let make_regular_method ?(visibility = `Public) name desc code =
  Method.(Regular { flags = [ visibility ];
                    name = make_method name;
                    descriptor = desc;
                    attributes = [ `Code code ]; })

let make_constructor_method desc code =
  Method.(Constructor { cstr_flags = [ `Public ];
                        cstr_descriptor = desc;
                        cstr_attributes = [ `Code code ]; })

let make_size_method name size =
  make_regular_method
    name
    ([], `Long)
    (make_code ~max_stack:2 ~max_locals:1
       [ Instruction.LDC2_W (`Long (Int64.of_int size));
         Instruction.LRETURN ])

let conv src dst =
  Bytecodegen_misc.compile_conversion ~relaxed_mode:true src dst
  |> Instrtree.flatten

let make_copy_method classname name elem_kind (elem_type : Descriptor.for_field) l =
  let l =
    l
    |> List.map
        (fun (idx, name, kind, typ) ->
          [ Instruction.ALOAD_1;
            Instruction.ILOAD_2;
            Instruction.LDC_W (`Int (Int32.of_int idx));
            Instruction.IADD;
            Instruction.ALOAD_0;
            Instruction.GETFIELD (classname, name, typ) ]
          @ (conv kind elem_kind)
          @ [ match elem_type with
          | `Int    -> Instruction.IASTORE
          | `Double -> Instruction.DASTORE
          | `Long   -> Instruction.LASTORE
          | _       -> Instruction.AASTORE ])
    |> List.flatten in
  let code = make_code ~max_stack:6 ~max_locals:4
      (l @ [ Instruction.RETURN ]) in
  make_regular_method name ([`Array elem_type; `Int], `Void) code

let make_copy_method' classname l =
  let l =
    l
    |> List.map
        (fun (idx, name, kind, typ) ->
          [ Instruction.ALOAD_0;
            Instruction.ALOAD_1;
            Instruction.LDC_W (`Int (Int32.of_int idx)) ]
          @ (Instrtree.flatten meth_get'int)
          @ (conv Boxed_value kind)
          @ [ Instruction.PUTFIELD (classname, name, typ) ])
    |> List.flatten in
  let code = make_code ~max_stack:5 ~max_locals:4
      (l @ [ Instruction.RETURN ]) in
  make_regular_method "copy" ([`Class class_BlockValue], `Void) code

let make_duplicate_method classname l =
  let l = 
    l
    |> List.map
        (fun (_, name, _, typ) ->
          [ Instruction.ALOAD_1;
            Instruction.ALOAD_0;
            Instruction.GETFIELD (classname, name, typ);
            Instruction.PUTFIELD (classname, name, typ) ])
    |> List.flatten in
  let code = make_code ~max_stack:5 ~max_locals:4
      ([ Instruction.NEW classname;
         Instruction.DUP;
         Instruction.LDC2_W (`Long (Int64.of_int (List.length l)));
         Instruction.INVOKESPECIAL (classname, make_method "<init>" , ([`Long], `Void));
         Instruction.ASTORE_1; ]
       @ l
       @ [ Instruction.ALOAD_1;
           Instruction.ARETURN ]) in
  make_regular_method "duplicate" ([], `Class class_Value) code

let return_zero = function
  | `Int    -> [ Instruction.ICONST_0;    Instruction.IRETURN ]
  | `Double -> [ Instruction.DCONST_0;    Instruction.DRETURN ]
  | `Long   -> [ Instruction.LCONST_0;    Instruction.LRETURN ]
  | _       -> [ Instruction.ACONST_NULL; Instruction.ARETURN ]

let return_instr = function
  | `Int    -> [ Instruction.IRETURN ]
  | `Double -> [ Instruction.DRETURN ]
  | `Long   -> [ Instruction.LRETURN ]
  | _       -> [ Instruction.ARETURN ]

let load_1_instr = function
  | `Int    -> Instruction.ILOAD_1
  | `Double -> Instruction.DLOAD_1
  | `Long   -> Instruction.LLOAD_1
  | _       -> Instruction.ALOAD_1

let load_2_instr = function
  | `Int    -> Instruction.ILOAD_2
  | `Double -> Instruction.DLOAD_2
  | `Long   -> Instruction.LLOAD_2
  | _       -> Instruction.ALOAD_2

let load_3_instr = function
  | `Int    -> Instruction.ILOAD_3
  | `Double -> Instruction.DLOAD_3
  | `Long   -> Instruction.LLOAD_3
  | _       -> Instruction.ALOAD_3

let make_switch_method make_instrs default_instrs is_set is_long (elem_type : Descriptor.for_field) method_name l =
  let switch_ofs = if is_long then 2 else 1 in
  let padding = 3 - (switch_ofs mod 4) in
  let switch_sz = 1 + padding + 4 + 4 + 4 + (4 * (List.length l)) in
  let start_ofs = switch_ofs + switch_sz in
  let default_ofs, offsets, cases, stack_map_frames =
    List.fold_left
      (fun (acc_ofs, acc_offsets, acc_cases, acc_map) (idx, name, kind, typ) ->
        let instrs = make_instrs idx name kind typ in
        let size = Instruction.size_of_list acc_ofs instrs in
        let frame = Attribute.Same_frame (u2 acc_ofs) in
        (acc_ofs + size, acc_ofs :: acc_offsets, instrs :: acc_cases, frame :: acc_map))
      (start_ofs, [], [], [])
      l in
  let offsets = List.rev_map (fun o -> o - switch_ofs) offsets in
  let cases = List.flatten (List.rev cases) in
  let stack_map_frames =
    List.rev @@
    (Attribute.Same_frame (u2 default_ofs)) :: stack_map_frames in
  let s4' x = s4 (Int32.of_int x) in
  let switch =
    Instruction.TABLESWITCH (s4' (default_ofs - switch_ofs),
                             s4' 0,
                             s4' ((List.length l) - 1),
                             List.map s4' offsets) in
  let code =
    make_code ~max_stack:5 ~max_locals:5
      ~attrs:(if cases <> [] then [ `StackMapTable stack_map_frames ] else [])
      ((if is_long then
        [Instruction.LLOAD_1 ; Instruction.L2I ]
      else
        [ Instruction.ILOAD_1 ])
       @ [if cases <> [] then switch else Instruction.POP]
       @ cases
       @ default_instrs) in
  let desc =
    if is_set then
      ((if is_long then `Long else `Int) :: [elem_type], `Void)
    else
      ([if is_long then `Long else `Int], (elem_type :> Descriptor.java_type)) in
  make_regular_method method_name desc code

let make_get_method is_long classname elem_kind (elem_type : Descriptor.for_field) method_name l =
  let make_instrs _ name kind typ =
    [ Instruction.ALOAD_0;
      Instruction.GETFIELD (classname, name, typ) ]
    @ (conv kind elem_kind)
    @ (return_instr elem_type) in
  let default_instrs = return_zero elem_type in
  make_switch_method make_instrs default_instrs false is_long elem_type method_name l

let make_set_method is_long classname elem_kind (elem_type : Descriptor.for_field) method_name l =
  let make_instrs _ name kind typ =
    [ Instruction.ALOAD_0;
      (if is_long then load_3_instr elem_type else load_2_instr elem_type) ]
    @ (conv elem_kind kind)
    @ [ Instruction.PUTFIELD (classname, name, typ) ]
    @ [ Instruction.RETURN ] in
  let default_instrs = [ Instruction.RETURN ] in
  make_switch_method make_instrs default_instrs true is_long elem_type method_name l

let make_get_and_set_methods classname elem_kind (elem_type : Descriptor.for_field) get_name set_name l =
  l
  |> List.mapi
      (fun i x ->
        let get_code, set_code =
          match x with
          | Some (_, name, kind, typ) ->
              let get_code =
                make_code ~max_stack:5 ~max_locals:1
                  ([ Instruction.ALOAD_0;
                     Instruction.GETFIELD (classname, name, typ) ]
                   @ (conv kind elem_kind)
                   @ (return_instr elem_type)) in
              let set_code =
                make_code ~max_stack:5 ~max_locals:3
                  ([ Instruction.ALOAD_0;
                     (load_1_instr elem_type) ]
                   @ (conv elem_kind kind)
                   @ [ Instruction.PUTFIELD (classname, name, typ) ]
                   @ [ Instruction.RETURN ]) in
              get_code, set_code
          | None ->
              let get_code =
                make_code ~max_stack:2 ~max_locals:1
                  (return_zero elem_type) in
              let set_code =
                make_code ~max_stack:2 ~max_locals:3
                  [ Instruction.RETURN ] in
              get_code, set_code in
        let get_meth =
          make_regular_method
            (get_name i)
            ([], (elem_type :> Descriptor.java_type))
            get_code in
        let set_meth =
          make_regular_method
            (set_name i)
            ([elem_type], `Void)
            set_code in
        get_meth, set_meth)
  |> List.split

let compile_class () =
  let classname = make_class (Jcompilenv.current_global_class_name ()) in
  let approx =
    match Jcompilenv.get_global_approx () with
    | Jlambda.Value_tuple x -> Array.to_list x
    | _ -> [] in
  let types =
    List.mapi
      (fun i app ->
        let name = make_field_name i in
        let kind, typ =
          match app with
          | Jlambda.Value_closure _ ->
              Boxed_value, `Class class_Value
          | Jlambda.Value_tuple _ ->
              Boxed_value, `Class class_Value
          | Jlambda.Value_unknown None ->
              Boxed_value, `Class class_Value
          | Jlambda.Value_unknown (Some repr) ->
              let open Lambda in
              begin match repr with
              | LR_value | LR_string | LR_exn | LR_array _
              | LR_list _ | LR_option _ | LR_lazy _ | LR_unit ->
                  Boxed_value, `Class class_Value
              | LR_int | LR_char | LR_bool ->
                  int_kind, `Long
              | LR_float ->
                  Unboxed_float, `Double
              | LR_nativeint ->
                  Unboxed_nativeint, `Long
              | LR_int32 ->
                  Unboxed_int32, `Int
              | LR_int64 ->
                  Unboxed_int64, `Long
              | LR_java_instance cn | LR_java_extends cn ->
                  Unboxed_instance cn, `Class (make_class cn)
              | (LR_java_boolean_array
              | LR_java_byte_array
              | LR_java_char_array
              | LR_java_double_array
              | LR_java_float_array
              | LR_java_int_array
              | LR_java_long_array
              | LR_java_reference_array _
              | LR_java_short_array) as arr ->
                  let arr = array_type_of_repr arr in
                  Unboxed_java_array arr, (Jlambda.unconvert_array_type arr :> Descriptor.for_field)
              | LR_none ->
                  assert false
              end
          | Jlambda.Value_integer _ ->
              int_kind, `Long
          | Jlambda.Value_integer32 _ ->
              Unboxed_int32, `Int
          | Jlambda.Value_integer64 _ ->
              Unboxed_int64, `Long
          | Jlambda.Value_integernat _ ->
              Unboxed_nativeint, `Long
          | Jlambda.Value_constptr _ ->
              int_kind, `Long
          | Jlambda.Value_float _ ->
              Unboxed_float, `Double
          | Jlambda.Value_java_null None ->
              Unboxed_instance "java.lang.Object", `Class (make_class "java.lang.Object")
          | Jlambda.Value_java_null (Some (`Class cn)) ->
              Unboxed_instance cn, `Class (make_class cn)
          | Jlambda.Value_java_null (Some ((`Array _) as arr)) ->
              Unboxed_java_array arr, (Jlambda.unconvert_array_type arr :> Descriptor.for_field)
          | Jlambda.Value_java_null _ ->
              assert false
          | Jlambda.Value_java_string _ ->
              Unboxed_instance "java.lang.String", `Class (make_class "java.lang.String") in
        i, name, kind, typ)
      approx in
  let fields =
    List.map
      (fun (_, name, _, descriptor) ->
        Field.({ flags = [ `Public ]; name; descriptor; attributes = []; }))
      types in
  let get_long_method = make_get_method true classname Boxed_value (`Class class_Value) "get" types in
  let get_int_method = make_get_method false classname Boxed_value (`Class class_Value) "get" types in
  let set_long_method = make_set_method true classname Boxed_value (`Class class_Value) "set" types in
  let set_int_method = make_set_method false classname Boxed_value  (`Class class_Value) "set" types in
  let getDouble_long_method = make_get_method true classname Unboxed_float `Double "getDouble" types in
  let getDouble_int_method = make_get_method false classname Unboxed_float `Double "getDouble" types in
  let setDouble_long_method = make_set_method true classname Unboxed_float `Double "setDouble" types in
  let setDouble_int_method = make_set_method false classname Unboxed_float `Double "setDouble" types in
  let getGenericDouble_long_method = make_get_method true classname Unboxed_float `Double "getGenericDouble" types in
  let getGenericDouble_int_method = make_get_method false classname Unboxed_float `Double "getGenericDouble" types in
  let setGenericDouble_long_method = make_set_method true classname Unboxed_float `Double "setGenericDouble" types in
  let setGenericDouble_int_method = make_set_method false classname Unboxed_float `Double "setGenericDouble" types in
  let getRawLong_long_method = make_get_method true classname int_kind `Long "getRawLong" types in
  let getRawLong_int_method = make_get_method false classname int_kind `Long "getRawLong" types in
  let setRawLong_long_method = make_set_method true classname int_kind `Long "setRawLong" types in
  let setRawLong_int_method = make_set_method false classname int_kind `Long "setRawLong" types in
  let getN_methods, setN_methods =
    make_get_and_set_methods
      classname
      Boxed_value
      (`Class class_Value)
      (Printf.sprintf "get%d")
      (Printf.sprintf "set%d")
      (at_most 8 types) in
  let getDoubleN_methods, setDoubleN_methods =
    make_get_and_set_methods
      classname
      Unboxed_float
      `Double
      (Printf.sprintf "getDouble%d")
      (Printf.sprintf "setDouble%d")
      (at_most 8 types) in
  let getGenericDoubleN_methods, setGenericDoubleN_methods =
    make_get_and_set_methods
      classname
      Unboxed_float
      `Double
      (Printf.sprintf "getGenericDouble%d")
      (Printf.sprintf "setGenericDouble%d")
      (at_most 8 types) in
  let getRawLongN_methods, setRawLongN_methods =
    make_get_and_set_methods
      classname
      int_kind
      `Long
      (Printf.sprintf "getRawLong%d")
      (Printf.sprintf "setRawLong%d")
      (at_most 8 types) in
  let size = List.length types in
  let size_methods =
    [ make_size_method "sizeValues" size;
      make_size_method "sizeLongs" size;
      make_size_method "sizeDoubles" size;
      make_size_method "arrayLength" size; ] in
  let truncateInstance_method =
    make_regular_method ~visibility:`Protected
      "truncateInstance"
      ([`Long], `Void)
      (make_code ~max_stack:0 ~max_locals:3
         [ Instruction.RETURN ]) in
  let copy_methods =
    [ make_copy_method classname "copyValuesIntoArray" Boxed_value (`Class class_Value) types;
      make_copy_method classname "copyDoublesIntoArray" Unboxed_float `Double types;
      make_copy_method classname "copyRawLongsIntoArray" int_kind `Long types; ] in
  let cstr =
    make_constructor_method
      []
      (make_code ~max_stack:4 ~max_locals:3
         ([ Instruction.ALOAD_0 ;
            Instruction.ICONST_0 ;
            Instruction.LDC2_W (`Long (Int64.of_int (List.length types))) ; ]
          @ (Instrtree.flatten cstr_GlobalValue)
          @ [ Instruction.RETURN ])) in
  let annotation = class_Global, [] in
  let cd = { ClassDefinition.access_flags = [`Public; `Final; `Super];
             name = classname;
             extends = Some class_GlobalValue;
             implements = [];
             fields = fields;
             methods = [cstr]
             @ [get_long_method; get_int_method] @ getN_methods
             @ [set_long_method; set_int_method] @ setN_methods
             @ [getDouble_long_method; getDouble_int_method] @ getDoubleN_methods
             @ [setDouble_long_method; setDouble_int_method] @ setDoubleN_methods
             @ [getGenericDouble_long_method; getGenericDouble_int_method] @ getGenericDoubleN_methods
             @ [setGenericDouble_long_method; setGenericDouble_int_method] @ setGenericDoubleN_methods
             @ [getRawLong_long_method; getRawLong_int_method] @ getRawLongN_methods
             @ [setRawLong_long_method; setRawLong_int_method] @ setRawLongN_methods
             @ size_methods
             @ [truncateInstance_method]
             @ copy_methods
             @ [make_copy_method' classname types; make_duplicate_method classname types];
             attributes = [ `RuntimeVisibleAnnotations [ annotation ] ]; } in
  let buff = ByteBuffer.make_of_size 2048 in
  let os = OutputStream.make_of_buffer buff in
  ClassFile.write (ClassDefinition.encode cd) os;
  OutputStream.close os;
  ByteBuffer.contents buff
