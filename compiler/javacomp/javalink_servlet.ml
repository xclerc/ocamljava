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
open Javalink_startup

let leaf = Instrtree.leaf

let flatten = Instrtree.flatten

let constructor_method =
  let constructor_code =
    leaf [ Instruction.ALOAD_0 ;
           Instruction.INVOKESPECIAL (make_class "java.lang.Object",
                                      make_method "<init>",
                                      ([], `Void)) ;
           Instruction.RETURN ] in
  Method.Constructor
    { Method.cstr_flags = [`Private];
      Method.cstr_descriptor = [];
      Method.cstr_attributes = [`Code { Attribute.max_stack = u2 2;
                                        Attribute.max_locals = u2 1;
                                        Attribute.code = flatten constructor_code;
                                        Attribute.exception_table = [];
                                        Attribute.attributes = []; }]; }

let make_servlet_class () =
  let main_class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_class in
  let main_class = make_class main_class_name in
  let class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_servlet_class in
  let done_field = { Field.flags = [`Private ; `Static];
                     name = make_field "done";
                     descriptor = `Boolean;
                     attributes = []; } in
  let static_code =
    leaf [ Instruction.ICONST_0 ;
           Instruction.PUTSTATIC (make_class class_name,
                                  make_field "done",
                                  `Boolean) ;
           Instruction.RETURN ] in
  let static_block =
    Method.Initializer
      { Method.init_flags = [`Static];
        Method.init_attributes = [`Code { Attribute.max_stack = u2 2;
                                          Attribute.max_locals = u2 1;
                                          Attribute.code = flatten static_code;
                                          Attribute.exception_table = [];
                                          Attribute.attributes = []; }]; } in
  let init_code =
    leaf [ Instruction.GETSTATIC (make_class class_name,
                                  make_field "done",
                                  `Boolean) ;
           Instruction.IFNE (s2 (1 + 3 + 3 + 1 + 3 + 3 + 1)) ;
           Instruction.ICONST_1 ;
           Instruction.PUTSTATIC (make_class class_name,
                                  make_field "done",
                                  `Boolean) ;
           Instruction.ICONST_0 ;
           Instruction.ANEWARRAY (`Class_or_interface class_String) ;
           Instruction.INVOKESTATIC (main_class,
                                     make_method "mainWithReturn",
                                     ([`Array (`Class class_String)], `Class main_class)) ;
           Instruction.POP ;
           Instruction.RETURN ] in
  let frames =
    [ Attribute.Full_frame (u2 0, [], []) ;
      Attribute.Same_frame (u2 18) ] in
  let init_method =
    Method.Regular
      { Method.flags = [ `Public; `Static; `Synchronized ];
  	Method.name = make_method "init";
  	Method.descriptor = ([], `Void);
  	Method.attributes = [`Code { Attribute.max_stack = u2 2;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = flatten init_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = [`StackMapTable frames] }]; } in
  let initialized_code =
    leaf [ Instruction.ICONST_1 ;
           Instruction.PUTSTATIC (make_class class_name,
                                  make_field "done",
                                  `Boolean) ;
           Instruction.RETURN ] in
  let initialized_method =
    Method.Regular
      { Method.flags = [ `Public; `Static; `Synchronized ];
  	Method.name = make_method "initialized";
  	Method.descriptor = ([], `Void);
  	Method.attributes = [`Code { Attribute.max_stack = u2 2;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = flatten initialized_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = [] }]; } in
  class_name,
  (compile_class
     ~name:(make_class class_name)
     ~parent:class_Object
     ~fields:[done_field]
     ~methods:[ static_block; constructor_method; init_method; initialized_method ]
     ~attributes:[])

let make_servlet_aux_class package =
  let main_class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_servlet_class in
  let class_name = package  ^ "." ^ Jconfig.main_servlet_class in
  let init_code =
    leaf [ Instruction.INVOKESTATIC (make_class main_class_name,
                                     make_method "init",
                                     ([], `Void)) ;
           Instruction.RETURN ] in
  let init_method =
    Method.Regular
      { Method.flags = [ `Public; `Static ];
  	Method.name = make_method "init";
  	Method.descriptor = ([], `Void);
  	Method.attributes = [`Code { Attribute.max_stack = u2 2;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = flatten init_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = [] }]; } in
  class_name,
  (compile_class
     ~name:(make_class class_name)
     ~parent:class_Object
     ~fields:[]
     ~methods:[ constructor_method; init_method ]
     ~attributes:[])

