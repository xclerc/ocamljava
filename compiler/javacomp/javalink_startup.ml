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

open Bytecodeutils
open BaristaLibrary
open Utils


let node = Instrtree.node

let leaf = Instrtree.leaf

let flatten = Instrtree.flatten

let compile_class ~name ~parent ~fields ~methods ~attributes =
  let cd = { ClassDefinition.access_flags = [`Public; `Final; `Super];
             name = name;
             extends = Some parent;
             implements = [];
             fields = fields;
             methods = methods;
             attributes = attributes; } in
  let buff = ByteBuffer.make_of_size 1024 in
  let stream = OutputStream.make_of_buffer buff in  
  ClassFile.write (ClassDefinition.encode cd) stream;
  OutputStream.close stream;
  ByteBuffer.contents buff

let make_startup_class units_to_link execname =
  let execname = Filename.basename execname in
  let class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_class in
  let main_class = make_class class_name in
  let classes =
    List.flatten
      (List.map
         (fun (infos, _, _) ->
           infos.Cmj_format.ui_additional_classes @ [infos.Cmj_format.ui_javaname])
         units_to_link) in
  let constructor_code =
    node
      [ leaf [ Instruction.ALOAD_0 ; Instruction.ALOAD_1 ] ;
        cstr_AbstractNativeRunner ;
        leaf [ Instruction.RETURN ] ] in
  let constructor_method =
    Method.Constructor
      { Method.cstr_flags = [`Private];
        Method.cstr_descriptor = [`Class interface_NativeParameters];
        Method.cstr_attributes = [`Code { Attribute.max_stack = u2 2;
                                          Attribute.max_locals = u2 2;
                                          Attribute.code = flatten constructor_code;
                                          Attribute.exception_table = [];
                                          Attribute.attributes = []; }]; } in
  let copy_constructor_code =
    node
      [ leaf [ Instruction.ALOAD_0 ; Instruction.ALOAD_1 ] ;
        cstr_copy_AbstractNativeRunner ;
        leaf [ Instruction.RETURN ] ] in
  let copy_constructor_method =
    Method.Constructor
      { Method.cstr_flags = [`Private];
        Method.cstr_descriptor = [`Class main_class];
        Method.cstr_attributes =  [`Code { Attribute.max_stack = u2 2;
                                           Attribute.max_locals = u2 2;
                                           Attribute.code = flatten copy_constructor_code;
                                           Attribute.exception_table = [];
                                           Attribute.attributes = []; }]; } in
  let copy_code =
    [ Instruction.NEW main_class ;
      Instruction.DUP ;
      Instruction.ALOAD_0 ;
      Instruction.INVOKESPECIAL (main_class,
                                 make_method "<init>",
                                 ([`Class main_class], `Void)) ;
      Instruction.ARETURN ] in
  let copy_method =
    Method.Regular
      { Method.flags = [`Public];
        Method.name = make_method "copy";
        Method.descriptor =  ([], `Class class_AbstractNativeRunner);
        Method.attributes = [`Code { Attribute.max_stack = u2 3;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = copy_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  let module_main_code =
    List.concat
      (List.map
         (fun c ->
           [ Instruction.ALOAD_0 ;
             Instruction.DUP ;
             Instruction.INVOKESTATIC
               (make_class c,
                make_method "entry",
                ([], `Class class_Value)) ;
             Instruction.PUTFIELD (class_AbstractNativeRunner,
                                   make_field "result",
                                   `Class class_Value) ;
             Instruction.INVOKEVIRTUAL
               (`Class_or_interface class_AbstractNativeRunner,
                make_method "incrGlobalsInited",
                ([], `Void)) ])
         classes)
    @ [ Instruction.RETURN ] in
  let module_main_method =
    Method.Regular
      { Method.flags = [`Protected];
        Method.name = make_method "moduleMain";
        Method.descriptor = ([], `Void);
        Method.attributes = [`Code { Attribute.max_stack = u2 3;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = module_main_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  let main_with_return_code =
    let bare_canvas =
      if !Jclflags.applet = Some Jclflags.Graphics then
        Instruction.ICONST_1
      else
        Instruction.ICONST_0 in
    let parameters =
      node
        [ leaf [ Instruction.LDC_W (`Class_or_interface main_class) ;
                 Instruction.LDC_W (`String (UTF8.of_string Jconfig.parameters_entry)) ] ;
          meth_getResourceAsStream ;
          leaf [ Instruction.ALOAD_0 ] ;
          field_System_in ;
          field_System_out ;
          field_System_err ;
          leaf [ bare_canvas ;
                 Instruction.LDC_W (`String (UTF8.of_string execname)) ;
                 Instruction.LDC_W (`Class_or_interface main_class) ] ;
          meth_fromStream ] in
    node
      [ leaf [ Instruction.NEW main_class ;
               Instruction.DUP ] ;
        parameters ;
        leaf [ Instruction.INVOKESPECIAL
                 (main_class,
                  make_method "<init>",
                  ([`Class interface_NativeParameters], `Void)) ;
               Instruction.ASTORE_1 ] ;
        node (List.map
                (fun c ->
                  let create =
                    Instruction.INVOKESTATIC
                      (make_class c,
                       make_method "createConstants",
                       ([], `Class (Bytecodegen_constants.const_class_of_curr_class c))) in
                  node
                    [ leaf [ Instruction.ALOAD_1 ;
                             Instruction.LDC_W (`Class_or_interface (make_class c)) ;
                             create ] ;
                      meth_setConstant ])
                classes) ;
        leaf [ Instruction.ALOAD_1 ] ;
        meth_execute ; (* calls moduleMain *)
        leaf [ Instruction.ALOAD_1 ;
               Instruction.ARETURN ] ] in
  let main_with_return_method =
    Method.Regular
      { Method.flags = [`Public; `Static];
        Method.name = make_method "mainWithReturn";
        Method.descriptor = ([`Array (`Class class_String)], `Class main_class);
        Method.attributes = [`Code { Attribute.max_stack = u2 11;
                                     Attribute.max_locals = u2 2;
                                     Attribute.code = flatten main_with_return_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  let main_scripting_code =
    let parameters =
      node
        [ leaf [ Instruction.LDC_W (`Class_or_interface main_class) ;
                 Instruction.LDC_W (`String (UTF8.of_string Jconfig.parameters_entry)) ] ;
          meth_getResourceAsStream ;
          leaf [ Instruction.ALOAD_0 ;
                 Instruction.ALOAD_2 ;
                 Instruction.ALOAD_3 ;
                 Instruction.ALOAD (u1 4) ;
                 Instruction.ICONST_0 ;
                 Instruction.LDC_W (`String (UTF8.of_string execname)) ;
                 Instruction.LDC_W (`Class_or_interface main_class) ] ;
          meth_fromStream ] in
    node
      [ leaf [ Instruction.NEW main_class ;
               Instruction.DUP ] ;
        parameters ;
        leaf  [ Instruction.INVOKESPECIAL
                  (main_class,
                   make_method "<init>",
                   ([`Class interface_NativeParameters], `Void)) ;
                Instruction.ASTORE (u1 5) ] ;
        node (List.map
                (fun c ->
                  let create =
                    Instruction.INVOKESTATIC
                      (make_class c,
                       make_method "createConstants",
                       ([], `Class (Bytecodegen_constants.const_class_of_curr_class c))) in
                  node
                    [ leaf [ Instruction.ALOAD (u1 5) ;
                             Instruction.LDC_W (`Class_or_interface (make_class c)) ;
                             create ] ;
                      meth_setConstant ])
                classes) ;
        leaf [  Instruction.ALOAD (u1 5) ;
                Instruction.ALOAD_1 ] ;
        meth_executeWithBindings ; (* calls moduleMain *)
        leaf [  Instruction.ALOAD (u1 5) ] ;
        meth_getResult ;
        leaf [ Instruction.ARETURN ] ] in
  let main_scripting_method =
    Method.Regular
      { Method.flags = [`Public; `Static];
        Method.name = make_method "mainScripting";
        Method.descriptor =
        ([`Array (`Class class_String);
          `Class class_Map;
          `Class class_InputStream;
          `Class class_PrintStream;
          `Class class_PrintStream], `Class class_Value);
        Method.attributes = [`Code { Attribute.max_stack = u2 11;
                                     Attribute.max_locals = u2 6;
                                     Attribute.code = flatten main_scripting_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  let main_code =
    node
      [ leaf
          [ Instruction.ALOAD_0 ;
            Instruction.INVOKESTATIC
              (main_class,
               make_method "mainWithReturn",
               ([`Array (`Class class_String)], `Class main_class)) ;
            Instruction.POP ] ;
        begin match !Jclflags.war with
        | Some _ ->
            leaf
              [ Instruction.INVOKESTATIC
                  (make_class (!Jclflags.java_package  ^ "." ^ Jconfig.main_servlet_class),
                   make_method "initialized",
                   ([], `Void)) ]
        | None ->
            leaf []
        end;
        leaf [ Instruction.RETURN ] ] in
  let main_method =
    Method.Regular
      { Method.flags = [`Public; `Static];
        Method.name = make_method "main";
        Method.descriptor = ([`Array (`Class class_String)], `Void);
        Method.attributes = [`Code { Attribute.max_stack = u2 1;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = flatten main_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  let annotation =
    class_EntryPoint,
    [ UTF8.of_string "standalone",
      Annotation.Boolean_value !Jclflags.standalone ;
      UTF8.of_string "linkedClasses",
      Annotation.Array_value
        (List.map
           (fun c -> Annotation.String_value (UTF8.of_string c))
           classes) ] in
  class_name,
  (compile_class
     ~name:main_class
     ~parent:class_AbstractNativeRunner
     ~fields:[]
     ~methods:[ constructor_method ;
                copy_constructor_method ;
                copy_method ;
                module_main_method ;
                main_with_return_method ;
                main_scripting_method ;
                main_method ]
     ~attributes:[ `RuntimeVisibleAnnotations [ annotation ] ])
