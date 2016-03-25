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
open BaristaLibrary
open Utils
open Javalink_startup

let node = Instrtree.node

let leaf = Instrtree.leaf

let flatten = Instrtree.flatten

let check_fxapplication_signature unit_infos cmj_file =
  let signature_cmi =
    Jcompilenv.check_signature
      unit_infos.Cmj_format.ui_name
      ((Filename.chop_extension cmj_file) ^ ".cmi")
      "JavaFX"
      "Application" in
  match unit_infos.Cmj_format.ui_approx with
  | Jlambda.Value_tuple x -> signature_cmi, x
  | _ -> failwith "invalid module"

type approx = {
    init : Jlambda.value_approximation * int;
    start : Jlambda.value_approximation * int;
    stop : Jlambda.value_approximation * int;
  }

let default_approx = {
    init = (Jlambda.Value_unknown None, ~-1);
    start = (Jlambda.Value_unknown None, ~-1);
    stop = (Jlambda.Value_unknown None, ~-1);
  }

let extract_fxapplication_approx signature approx =
  let res, _, _ =
    List.fold_left
      (fun (current_approx, approx, idx) elem ->
        match elem with
        | Types.Sig_value (path, _) ->
            let name = Ident.name path in
            let value, approx =
              match approx with
              | hd :: tl -> (hd, idx), tl
              | [] -> assert false in
            let idx = succ idx in
            (match name with
            | "init" ->
                { current_approx with init = value }, approx, idx
            | "start" ->
                { current_approx with start = value }, approx, idx
            | "stop" ->
                { current_approx with stop = value }, approx, idx
            | _ ->
                current_approx, approx, idx)
        | Types.Sig_exception _ | Types.Sig_module _ | Types.Sig_class _ ->
            let approx = match approx with _ :: tl -> tl | [] -> assert false in
            current_approx, approx, succ idx
        | Types.Sig_type _ | Types.Sig_modtype _ | Types.Sig_class_type _ ->
            current_approx, approx, idx)
      (default_approx, approx, 0)
      signature in
  res

let make_fxapplication_class unit_infos approx =
  let main_class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_class in
  let main_class = make_class main_class_name in
  let class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_javafx_application_class in
  let parent_class = make_class "javafx.application.Application" in
  let super_constructor =
    Instruction.INVOKESPECIAL (parent_class,
                               make_method "<init>",
                               ([], `Void)) in
  let constructor_code =
    node
      [ leaf [ Instruction.ALOAD_0 ;
               super_constructor ] ;
        leaf [ Instruction.ICONST_0 ;
               Instruction.ANEWARRAY (`Class_or_interface class_String) ;
               Instruction.INVOKESTATIC
                 (main_class,
                  make_method "mainWithReturn",
                  ([`Array (`Class class_String)], `Class main_class)) ] ;
        leaf [ Instruction.INVOKESTATIC (make_class "java.lang.Thread",
                                         make_method "currentThread",
                                         ([], `Class (make_class "java.lang.Thread"))) ;
               Instruction.SWAP ;
               Instruction.INVOKESTATIC (make_class "org.ocamljava.runtime.kernel.OCamlJavaThread",
                                         make_method "registerCodeRunner",
                                         ([`Class (make_class "java.lang.Thread");
                                           `Class (make_class "org.ocamljava.runtime.kernel.CodeRunner")], `Void)) ] ;
        leaf [ Instruction.RETURN ] ] in
  let constructor_method =
    Method.Constructor
      { Method.cstr_flags = [`Public];
        Method.cstr_descriptor = [];
        Method.cstr_attributes = [`Code { Attribute.max_stack = u2 3;
                                          Attribute.max_locals = u2 1;
                                          Attribute.code = flatten constructor_code;
                                          Attribute.exception_table = [];
                                          Attribute.attributes = []; }]; } in


  let call_function ~call_parent name (approx, idx) ~stage_param =
    match approx with
    | Jlambda.Value_closure ({ Jlambda.fun_label = { Jlambda.fl_class = class_name;
                                                     fl_method = meth_name };
                               fun_closed = closed; _ },
                             _) ->
      let meth_params =
        if stage_param then
          [`Class (make_class "javafx.stage.Stage")]
        else
          [] in
      let params =
        match stage_param, not closed with
        | false, false -> [`Class class_Value]
        | false, true  -> [`Class class_Value; `Class class_Value]
        | true,  false -> [`Class class_Value; `Class class_Value]
        | true,  true  -> [`Class class_Value; `Class class_Value; `Class class_Value] in
      let code =
        (* note: do not use unboxed parameters, because the developer
           may have provided a compatible module that do not refer to
           unboxed type (e.g. "let stop _ = ...") *)
        node
          [ (* call parent code *)
            (if call_parent = `Before then
              node
                [ leaf [ Instruction.ALOAD_0 ] ;
                  (if stage_param then leaf [ Instruction.ALOAD_1 ] else leaf []) ;
                  leaf [ Instruction.INVOKESPECIAL (parent_class,
                                                    make_method name,
                                                    (meth_params, `Void)) ] ]
            else
              leaf []) ;

            (* call OCaml code *)
            node
              [ leaf [ Instruction.ALOAD_0 ] ;
                meth_createInstance ] ;
            (if stage_param then
              node
                [ leaf [ Instruction.ALOAD_1 ] ;
                  meth_createInstance ]
            else
              leaf []) ;
            (if closed then
              leaf []
            else
              leaf
                [ Instruction.INVOKESTATIC (make_class unit_infos.Cmj_format.ui_javaname,
                                            make_method "getGlobal",
                                            ([], `Class (make_class (unit_infos.Cmj_format.ui_javaname ^ "$Global")))) ;
                  Instruction.LDC2_W (`Long (Int64.of_int idx)) ;
                  Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                             make_method "get",
                                             ([`Long], `Class class_Value)) ]) ;
            leaf [ Instruction.INVOKESTATIC (make_class class_name,
                                             make_method meth_name,
                                             (params, `Class class_Value)) ] ;

            (* call parent code *)
            (if call_parent = `After then
              node
                [ leaf [ Instruction.ALOAD_0 ] ;
                  (if stage_param then leaf [ Instruction.ALOAD_1 ] else leaf []) ;
                  leaf [ Instruction.INVOKESPECIAL (parent_class,
                                                    make_method name,
                                                    (meth_params, `Void)) ] ]
            else
              leaf []) ;
            (* return *)
            leaf [ Instruction.RETURN ] ] in
      Method.Regular
        { Method.flags = [ `Public ];
  	  Method.name = make_method name;
  	  Method.descriptor = (meth_params, `Void);
  	  Method.attributes = [`Code { Attribute.max_stack = u2 8;
                                       Attribute.max_locals = u2 2;
                                       Attribute.code = flatten code;
                                       Attribute.exception_table = [];
                                       Attribute.attributes = []; }]; }
    | _ ->
        assert false in
  let main_code =
    node [
      leaf [ Instruction.ALOAD_0 ;
             Instruction.INVOKESTATIC (make_class class_name,
                                       make_method "launch",
                                       ([`Array (`Class class_String)], `Void)) ;
             Instruction.RETURN ] ] in
  let main_method =
    Method.Regular
      { Method.flags = [ `Public; `Static ];
  	Method.name = make_method "main";
  	Method.descriptor = ([`Array (`Class class_String)], `Void);
  	Method.attributes = [`Code { Attribute.max_stack = u2 8;
                                     Attribute.max_locals = u2 2;
                                     Attribute.code = flatten main_code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  class_name,
  (compile_class
     ~name:(make_class class_name)
     ~parent:parent_class
     ~fields:[]
     ~methods:([ constructor_method ;
                 call_function ~call_parent:`Before "init" approx.init ~stage_param:false;
                 call_function ~call_parent:`Never "start" approx.start ~stage_param:true;
                 call_function ~call_parent:`After "stop" approx.stop ~stage_param:false;
                 main_method ])
     ~attributes:[])
