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

open Lambda
open Asttypes
open BaristaLibrary
open Utils
open Bytecodegen_misc


module StringMap = Map.Make (String)

let field_name_of_index idx =
  Bytecodeutils.make_field (Printf.sprintf "constant_%d" idx)

let const_class_of_curr_class curr_class =
  Bytecodeutils.make_class (curr_class ^ "$Constants")


(* Atomic constants (int, int32, int64, nativeint, and float) *)

type atomic_constant_info = {
    field : Field.t;
    init : Instrtree.t;
  }

let atomic_constants : atomic_constant_info StringMap.t ref = ref StringMap.empty

let add_atomic_constant identifier push_value =
  let field_name = Bytecodeutils.make_field identifier in
  let class_name = Bytecodeutils.make_class (State.current_class ()) in
  let typ = `Class Bytecodeutils.class_Value in
  if not (StringMap.mem identifier !atomic_constants) then begin
    let field = Field.({ flags = [ `Public; `Static; `Final ];
                         name = field_name;
                         descriptor = `Class Bytecodeutils.class_Value;
                         attributes = []; }) in
    let init =
      Instrtree.node
        [ push_value ;
          Instrtree.leaf [ Instruction.PUTSTATIC (class_name, field_name, typ) ] ] in
    atomic_constants := StringMap.add identifier { field; init } !atomic_constants
  end;
  Instrtree.leaf [ Instruction.GETSTATIC (class_name, field_name, typ) ]

let push_int x =
  match x with
  | -2L -> Bytecodeutils.field_Value_MINUS_TWO
  | -1L -> Bytecodeutils.field_Value_MINUS_ONE
  | 0L  -> Bytecodeutils.field_Value_ZERO
  | 1L  -> Bytecodeutils.field_Value_ONE
  | 2L  -> Bytecodeutils.field_Value_TWO
  | 3L  -> Bytecodeutils.field_Value_THREE
  | 4L  -> Bytecodeutils.field_Value_FOUR
  | 5L  -> Bytecodeutils.field_Value_FIVE
  | 6L  -> Bytecodeutils.field_Value_SIX
  | 7L  -> Bytecodeutils.field_Value_SEVEN
  | 8L  -> Bytecodeutils.field_Value_EIGHT
  | _   ->
      Instrtree.node
        [ Bytecodeutils.push_int64 x ;
          Bytecodeutils.meth_createLong ]
      |> add_atomic_constant (Printf.sprintf "INT_%016LX" x)

let push_int32 x =
  Instrtree.node
    [ Bytecodeutils.push_int32 x ;
      Bytecodeutils.meth_createInt32 ]
  |> add_atomic_constant (Printf.sprintf "INT32_%08lX" x)

let push_int64 x =
  Instrtree.node
    [ Bytecodeutils.push_int64 x ;
      Bytecodeutils.meth_createInt64 ]
  |> add_atomic_constant (Printf.sprintf "INT64_%016LX" x)

let push_nativeint x =
  Instrtree.node
    [ Bytecodeutils.push_int64 (Int64.of_nativeint x) ;
      Bytecodeutils.meth_createNativeInt ]
  |> add_atomic_constant (Printf.sprintf "NATIVEINT_%016nX" x)

let push_float x =
  Instrtree.node
    [ Bytecodeutils.push_double x ;
      Bytecodeutils.meth_createDouble ]
  |> add_atomic_constant (Printf.sprintf "FLOAT_%016LX" (Int64.bits_of_float x))

let get_fields_and_inits () =
  let fields, inits =
    !atomic_constants
    |> StringMap.bindings
    |> List.map (fun (_, v) -> v.field, Instrtree.flatten v.init)
    |> List.split in
  fields, List.flatten inits


(* structured constants *)

let structured_constants : Lambda.structured_constant list ref = ref []

let add_structured_constant c =
  let res = List.length !structured_constants in
  structured_constants := c :: !structured_constants;
  res

let push_structured_constant c =
  match c with
  | Const_base (Const_int i) ->
      Bytecodeutils.push_int i
  | Const_base (Const_char c) ->
      Bytecodeutils.push_int (Char.code c)
  | Const_base (Const_float s) ->
      Bytecodeutils.push_double (float_of_string s)
  | Const_base (Const_int32 i) ->
      Bytecodeutils.push_int32 i
  | Const_base (Const_int64 i) ->
      Bytecodeutils.push_int64 i
  | Const_base (Const_nativeint i) ->
      Bytecodeutils.push_int64 (Int64.of_nativeint i)
  | Const_pointer i ->
      push_int (Int64.of_int i)
  | Const_base (Const_string _)
  | Const_immstring _
  | Const_block _
  | Const_float_array _ ->
      let const_class = const_class_of_curr_class (State.current_class ()) in
      let curr_class = Bytecodeutils.make_class (State.current_class ()) in
      let idx = add_structured_constant c in
      let field_name = field_name_of_index idx in
      Instrtree.node
        [ Instrtree.leaf [ Instruction.GETSTATIC (curr_class,
                                                  Bytecodeutils.make_field "CONSTANTS",
                                                  `Class Bytecodeutils.class_ThreadLocal) ] ;
          Bytecodeutils.meth_get_threadlocal ;
          Instrtree.leaf [ Instruction.CHECKCAST (`Class_or_interface const_class) ;
                           Instruction.GETFIELD (const_class,
                                                 field_name,
                                                 `Class Bytecodeutils.class_Value) ] ]

let marshal_string_of_structured_constants () =
  let rec conv = function
    | Const_base (Const_int i)       -> Obj.repr i
    | Const_base (Const_char c)      -> Obj.repr c
    | Const_base (Const_string s)    -> Obj.repr s
    | Const_base (Const_float s)     -> Obj.repr (float_of_string s)
    | Const_base (Const_int32 i)     -> Obj.repr i
    | Const_base (Const_int64 i)     -> Obj.repr i
    | Const_base (Const_nativeint i) -> Obj.repr i
    | Const_pointer i                -> Obj.repr i
    | Const_immstring s              -> Obj.repr s
    | Const_block (tag, fields)      ->
        let block = Obj.new_block tag (List.length fields) in
        List.iteri
          (fun pos c ->
            Obj.set_field block pos (conv c))
          fields;
        block
    | Const_float_array fields ->
        Obj.repr (Array.of_list (List.map float_of_string fields)) in
  let towrite = List.rev_map conv !structured_constants in
  let array = Array.make (List.length towrite) (Obj.magic 0) in
  List.iteri
    (fun i c -> array.(i) <- c)
    towrite;
  Marshal.to_string array []
  |> Bytes.make_of_string

let reset () =
  structured_constants := [];
  atomic_constants := StringMap.empty

let path_of_class_name x =
  let res = String.copy x in
  for i = 0 to pred (String.length res) do
    if res.[i] = '.' then res.[i] <- '/'
  done;
  res

let compile_class needs_marshalled_data =
  let cstr =
    let instrs =
      Instrtree.leaf [ Instruction.ALOAD_0 ;
                       Instruction.INVOKESPECIAL (Bytecodeutils.class_Object,
                                                  Bytecodeutils.make_method "<init>",
                                                  ([], `Void)) ;
                       Instruction.RETURN ] in
    let code =
      Attribute.({ max_stack = u2 1;
                   max_locals = u2 1;
                   code = (Instrtree.flatten instrs);
                   exception_table = [];
                   attributes = []; }) in
    Method.(Constructor { cstr_flags = [ `Public ];
                          cstr_descriptor = [];
                          cstr_attributes = [ `Code code ]; }) in
  let const_class = const_class_of_curr_class (State.current_class ()) in
  let annotation = Bytecodeutils.class_Constants, [] in
  let fields =
    List.mapi
      (fun idx _ ->
        let field_name = field_name_of_index idx in
        Field.({ flags = [ `Public ];
                 name = field_name;
                 descriptor = `Class Bytecodeutils.class_Value;
                 attributes = []; }))
    !structured_constants in
  let cd =
    ClassDefinition.({ access_flags = [ `Public; `Final; `Super ];
                       name = const_class;
                       extends = Some Bytecodeutils.class_Object;
                       implements = [];
                       fields = fields;
                       methods = [cstr];
                       attributes = [ `RuntimeVisibleAnnotations [ annotation ] ]; }) in
  let buff = ByteBuffer.make_of_size 2048 in
  let os = OutputStream.make_of_buffer buff in
  ClassFile.write (ClassDefinition.encode cd) os;
  OutputStream.close os;
  let class_path = UTF8.to_string (Name.internal_utf8_for_class const_class) in
  let class_path = class_path ^ Jconfig.ext_class in
  (class_path, ByteBuffer.contents buff)
  :: (if needs_marshalled_data then
    [((path_of_class_name (State.current_class ()))  ^ Jconfig.ext_consts),
     (marshal_string_of_structured_constants ())]
  else
    [])

let init_class_fields_from_code () =
  let rec push = function
    | Const_base (Const_int i)
    | Const_pointer i ->
        push_int (Int64.of_int i)
    | Const_base (Const_char c) ->
        push_int (Int64.of_int (Char.code c))
    | Const_base (Const_string s)
    | Const_immstring s ->
        Instrtree.node
          [ Instrtree.leaf
              [ Instruction.LDC_W (`String (UTF8.of_string s)) ] ;
            Bytecodeutils.meth_createString ]
    | Const_base (Const_float s) ->
        push_float (float_of_string s)
    | Const_base (Const_int32 i) ->
        push_int32 i
    | Const_base (Const_int64 i) ->
        push_int64 i
    | Const_base (Const_nativeint i) ->
        push_nativeint i
    | Const_block (tag, fields) ->
        let sz = List.length fields in
        if sz <= 8 then begin
          Instrtree.node
            [ Bytecodeutils.push_int32 (Int32.of_int tag) ;
              Instrtree.node (List.map push fields) ;
              Instrtree.leaf
                [ Instruction.INVOKESTATIC
                    (Bytecodeutils.class_Value,
                     Bytecodeutils.make_method "createBlock",
                     (`Int :: (repeat_parameters sz), `Class Bytecodeutils.class_Value)) ] ]
        end else begin
          Instrtree.node
            [ Bytecodeutils.push_int32 (Int32.of_int tag) ;
              Bytecodeutils.push_int64 (Int64.of_int sz) ;
              Bytecodeutils.meth_createBlock ;
              Instrtree.node
                (List.mapi
                   (fun i f ->
                     Instrtree.node
                       [ Instrtree.leaf [ Instruction.DUP ] ;
                         Bytecodeutils.push_int32 (Int32.of_int i) ;
                         push f ;
                         Bytecodeutils.meth_set'int ])
                   fields) ]
        end
    | Const_float_array fields ->
        let sz = List.length fields in
        if sz <= 8 then begin
          Instrtree.node
            [ Instrtree.node (List.map (fun x -> Bytecodeutils.push_double (float_of_string x)) fields) ;
              Instrtree.leaf
                [ Instruction.INVOKESTATIC
                    (Bytecodeutils.class_Value,
                     Bytecodeutils.make_method "createDoubleArray",
                     (repeat_doubles sz, `Class Bytecodeutils.class_Value)) ] ]
        end else begin
          Instrtree.node
            [ Bytecodeutils.push_int64 (Int64.of_int sz) ;
              Bytecodeutils.meth_createDoubleArray ;
              Instrtree.node
                (List.mapi
                   (fun i f ->
                     Instrtree.node
                       [ Instrtree.leaf [ Instruction.DUP ] ;
                         Bytecodeutils.push_int32 (Int32.of_int i) ;
                         Bytecodeutils.push_double (float_of_string f) ;
                         Bytecodeutils.meth_setDouble'int ])
                   fields) ]
        end in
  let const_class = const_class_of_curr_class (State.current_class ()) in
  let l =
    !structured_constants
    |> List.rev
    |> List.mapi
        (fun idx const ->
          let field_name = field_name_of_index idx in
          Instrtree.node
            [ Instrtree.leaf [ Instruction.ALOAD_0 ] ;
              push const ;
              Instrtree.leaf [ Instruction.PUTFIELD (const_class,
                                                     field_name,
                                                     `Class Bytecodeutils.class_Value) ] ]) in
  [ false,
    "createConstants",
    Instrtree.node
      [ Instrtree.leaf [ Instruction.NEW const_class ;
                         Instruction.DUP ;
                         Instruction.INVOKESPECIAL (const_class,
                                                    Bytecodeutils.make_method "<init>",
                                                    ([], `Void)) ;
                         Instruction.ASTORE_0 ] ;
        Instrtree.node l ;
        Instrtree.leaf [ Instruction.ALOAD_0 ;
                         Instruction.ARETURN ] ] ]

let init_class_fields_from_load () =
  let const_class = const_class_of_curr_class (State.current_class ()) in
  let curr_class = Bytecodeutils.make_class (State.current_class ()) in
  let prefix_instrs =
    Instrtree.node
      [ Instrtree.leaf [ Instruction.NEW const_class ;
                         Instruction.DUP ;
                         Instruction.INVOKESPECIAL (const_class,
                                                    Bytecodeutils.make_method "<init>",
                                                    ([], `Void)) ;
                         Instruction.ASTORE_0 ] ;
        Instrtree.leaf [ Instruction.LDC_W (`Class_or_interface curr_class) ] ;
        Bytecodeutils.meth_loadConstants ;
        Instrtree.leaf [ Instruction.ASTORE_1 ] ] in
  let instrs =
    !structured_constants
    |> List.rev
    |> List.mapi
        (fun idx _ ->
          let field_name = field_name_of_index idx in
          Instrtree.node
            [ Instrtree.leaf [ Instruction.ALOAD_0 ;
                               Instruction.ALOAD_1 ] ;
              Bytecodeutils.push_int64 (Int64.of_int idx) ;
              Bytecodeutils.meth_get ;
              Instrtree.leaf [ Instruction.PUTFIELD (const_class,
                                                     field_name,
                                                     `Class Bytecodeutils.class_Value) ] ]) in
  let suffix_intrs = Instrtree.leaf [ Instruction.ALOAD_0 ;
                                      Instruction.ARETURN ] in
  [ false,
    "createConstants",
    Instrtree.node
      [ prefix_instrs ;
        Instrtree.node instrs ;
        suffix_intrs ] ]

let split_list n l =
  let rec take acc n = function
    | (hd :: tl) as l ->
        if n <= 0 then
          List.rev acc, l
        else
          take (hd :: acc) (pred n) tl
    | [] -> List.rev acc, [] in
  let rec iter idx acc n = function
    | (_ :: _) as l ->
        let l1, l2 = take [] n l in
        iter (idx + List.length l1) ((idx, l1) :: acc) n l2
    | [] -> List.rev acc in
  iter 0 [] n l

let init_class_fields_from_split_load_n n =
  let const_class = const_class_of_curr_class (State.current_class ()) in
  let curr_class = Bytecodeutils.make_class (State.current_class ()) in
  let prefix_instrs =
    Instrtree.node
      [ Instrtree.leaf [ Instruction.NEW const_class ;
                         Instruction.DUP ;
                         Instruction.INVOKESPECIAL (const_class,
                                                    Bytecodeutils.make_method "<init>",
                                                    ([], `Void)) ;
                         Instruction.ASTORE_0 ] ;
        Instrtree.leaf [ Instruction.LDC_W (`Class_or_interface curr_class) ] ;
        Bytecodeutils.meth_loadConstants ;
        Instrtree.leaf [ Instruction.ASTORE_1 ] ] in
  let len = List.length !structured_constants in
  let parts = split_list (succ (len / n)) (List.rev !structured_constants) in
  let auxiliary_methods =
    List.mapi
      (fun meth_idx (base_idx, struct_consts) ->
        let instrs =
          List.mapi
            (fun const_idx _ ->
              let const_idx = const_idx + base_idx in
              let field_name = field_name_of_index const_idx in
              Instrtree.node
                [ Instrtree.leaf [ Instruction.ALOAD_0 ;
                                   Instruction.ALOAD_1 ] ;
                  Bytecodeutils.push_int64 (Int64.of_int const_idx) ;
                  Bytecodeutils.meth_get ;
                  Instrtree.leaf [ Instruction.PUTFIELD (const_class,
                                                         field_name,
                                                         `Class Bytecodeutils.class_Value) ] ])
            struct_consts in
        true,
        "createConstants" ^ string_of_int meth_idx,
        Instrtree.node
          [ Instrtree.node instrs ;
            Instrtree.leaf [ Instruction.RETURN ] ])
      parts in
  let instrs =
    List.mapi
      (fun idx _ ->
        Instrtree.leaf
          [ Instruction.ALOAD_0 ;
            Instruction.ALOAD_1 ;
            Instruction.INVOKESTATIC
              (curr_class,
               Bytecodeutils.make_method ("createConstants" ^ string_of_int idx),
               ([`Class const_class; `Class Bytecodeutils.class_Value], `Void)) ])
      parts in
  let suffix_intrs = Instrtree.leaf [ Instruction.ALOAD_0 ;
                                      Instruction.ARETURN ] in
  (false,
   "createConstants",
   Instrtree.node
     [ prefix_instrs ;
       Instrtree.node instrs ;
       suffix_intrs ])
  :: auxiliary_methods

let init_class_fields_from_split_load () =
  let rec iter n =
    let res = init_class_fields_from_split_load_n n in
    if List.for_all (fun (_, _, instrs) -> Instrtree.size instrs < 65536) res then
      res
    else
      iter (succ n) in
  iter 2
