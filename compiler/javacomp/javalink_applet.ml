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

let node = Instrtree.node

let leaf = Instrtree.leaf

let flatten = Instrtree.flatten

let check_applet_signature unit_infos cmj_file =
  let kind =
    match !Jclflags.applet with
    | Some Jclflags.Graphics -> "Graphics"
    | Some Jclflags.Awt -> "AWT"
    | Some Jclflags.Swing -> "Swing"
    | None -> assert false in
  let signature_cmi =
    Jcompilenv.check_signature
      unit_infos.Cmj_format.ui_name
      ((Filename.chop_extension cmj_file) ^ ".cmi")
      "JavaApplet"
      kind in
  match unit_infos.Cmj_format.ui_approx with
  | Jlambda.Value_tuple x -> signature_cmi, x
  | _ -> failwith kind

type approx = {
    applet_info : Jlambda.value_approximation * int;
    parameter_info : Jlambda.value_approximation * int;
    init : Jlambda.value_approximation * int;
    start : Jlambda.value_approximation * int;
    run : Jlambda.value_approximation * int;
    stop : Jlambda.value_approximation * int;
    destroy : Jlambda.value_approximation * int;
  }

let default_approx = {
  applet_info = (Jlambda.Value_unknown None, ~-1);
  parameter_info = (Jlambda.Value_unknown None, ~-1);
  init = (Jlambda.Value_unknown None, ~-1);
  start = (Jlambda.Value_unknown None, ~-1);
  run = (Jlambda.Value_unknown None, ~-1);
  stop = (Jlambda.Value_unknown None, ~-1);
  destroy = (Jlambda.Value_unknown None, ~-1);
}

let extract_applet_approx signature approx =
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
            | "applet_info" ->
                { current_approx with applet_info = value }, approx, idx
            | "parameter_info" ->
                { current_approx with parameter_info = value }, approx, idx
            | "init" ->
                { current_approx with init = value }, approx, idx
            | "start" ->
                { current_approx with start = value }, approx, idx
            | "run" ->
                { current_approx with run = value }, approx, idx
            | "stop" ->
                { current_approx with stop = value }, approx, idx
            | "destroy" ->
                { current_approx with destroy = value }, approx, idx
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

let make_applet_class unit_infos approx =
  let main_class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_class in
  let main_class = make_class main_class_name in
  let class_name = !Jclflags.java_package  ^ "." ^ Jconfig.main_applet_class in
  let parent_class, super_constructor =
    match !Jclflags.applet with
    | Some Jclflags.Graphics ->
        make_class "org.ocamljava.runtime.support.applet.GraphicsApplet",
        Instruction.INVOKESPECIAL (make_class "org.ocamljava.runtime.support.applet.GraphicsApplet",
                                   make_method "<init>",
                                   ([], `Void))
    | Some Jclflags.Awt ->
        make_class "java.applet.Applet",
        Instruction.INVOKESPECIAL (make_class "java.applet.Applet",
                                   make_method "<init>",
                                   ([], `Void))
    | Some Jclflags.Swing ->
        make_class "javax.swing.JApplet",
        Instruction.INVOKESPECIAL (make_class "javax.swing.JApplet",
                                   make_method "<init>",
                                   ([], `Void))
    | _ -> assert false in
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
  let call_function ~opening name (approx, idx) ~param =
    match approx with
    | Jlambda.Value_closure ({ Jlambda.fun_label = { Jlambda.fl_class = class_name;
                                                     fl_method = meth_name };
                               fun_repr_parameters = repr_parameters;
                               fun_closed = closed; _ },
                             _) ->
      let param =
        if param = `None then
          match repr_parameters with
          | Lambda.LR_value :: _ -> `Unit
          | _ -> param
        else
          param in
      let code =
        (* note: do not use unboxed parameters, because the developer
           may have provided a compatible module that do not refer to
           unboxed type (e.g. "let stop _ = ...") *)
        let params =
          if closed then
            [`Class class_Value]
          else
            [`Class class_Value; `Class class_Value] in
        node
          [ (* call parent code *)
            (if opening then
              node
                [ leaf [ Instruction.ALOAD_0 ] ;
                  (if param = `Event then leaf [ Instruction.ALOAD_1 ] else leaf []) ;
                  leaf [ Instruction.INVOKESPECIAL (parent_class,
                                                    make_method name,
                                                    ((if param = `Event then [`Class class_Value] else []), `Void)) ] ]
            else
              leaf []) ;
            (* call OCaml code *)
            (match param with
            | `None ->
                field_Value_UNIT
            | `Unit ->
                field_Value_UNIT
            | `Applet | `JApplet ->
                node
                  [ leaf [ Instruction.ALOAD_0 ] ;
                     meth_createInstance ]
            | `Event ->
                leaf [ Instruction.ALOAD_1 ]) ;
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
            (if not opening then
              node
                [ leaf [ Instruction.ALOAD_0 ] ;
                  (if param = `Event then leaf [ Instruction.ALOAD_1 ] else leaf []) ;
                  leaf [ Instruction.INVOKESPECIAL (parent_class,
                                                    make_method name,
                                                    ((if param = `Event then [`Class class_Value] else []), `Void)) ] ]
            else
              leaf []) ;
            (* return *)
            leaf [ Instruction.RETURN ] ] in
      Method.Regular
        { Method.flags = [ `Public ];
  	  Method.name = make_method name;
  	  Method.descriptor = ((if param = `Event then [`Class class_Value] else []), `Void);
  	  Method.attributes = [`Code { Attribute.max_stack = u2 8;
                                       Attribute.max_locals = u2 2;
                                       Attribute.code = flatten code;
                                       Attribute.exception_table = [];
                                       Attribute.attributes = []; }]; }
    | _ -> assert false in
  let get_value name (_, idx) typ =
    let code =
      leaf
        [ Instruction.INVOKESTATIC (make_class unit_infos.Cmj_format.ui_javaname,
                                    make_method "getGlobal",
                                    ([], `Class (make_class (unit_infos.Cmj_format.ui_javaname ^ "$Global")))) ;
          Instruction.LDC2_W (`Long (Int64.of_int idx)) ;
          Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                     make_method "get",
                                     ([`Long], `Class class_Value)) ;
          Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                     make_method "asCustom",
                                     ([], `Class class_Object)) ;
          (match typ with
          | `String ->
              Instruction.CHECKCAST (`Class_or_interface class_String)
          | `StringArray2 ->
              Instruction.CHECKCAST (`Array_type (`Array (`Array (`Class class_String))))) ;
          Instruction.ARETURN ] in
    let ret =
      match typ with
      | `String -> `Class class_String
      | `StringArray2 -> `Array (`Array (`Class class_String)) in
     Method.Regular
      { Method.flags = [ `Public ];
  	Method.name = make_method name;
  	Method.descriptor = ([], ret);
  	Method.attributes = [`Code { Attribute.max_stack = u2 3;
                                     Attribute.max_locals = u2 1;
                                     Attribute.code = flatten code;
                                     Attribute.exception_table = [];
                                     Attribute.attributes = []; }]; } in
  let constructor_method =
    Method.Constructor
      { Method.cstr_flags = [`Public];
        Method.cstr_descriptor = [];
        Method.cstr_attributes = [`Code { Attribute.max_stack = u2 3;
                                          Attribute.max_locals = u2 1;
                                          Attribute.code = flatten constructor_code;
                                          Attribute.exception_table = [];
                                          Attribute.attributes = []; }]; } in
  let param =
    match !Jclflags.applet with
    | Some Jclflags.Graphics -> `None
    | Some Jclflags.Awt -> `Applet
    | Some Jclflags.Swing -> `JApplet
    | None -> assert false in
  class_name,
  (compile_class
     ~name:(make_class class_name)
     ~parent:parent_class
     ~fields:[]
     ~methods:([ constructor_method ;
                 call_function ~opening:true "init" approx.init ~param;
                 call_function ~opening:true "start" approx.start ~param;
                 call_function ~opening:false "stop" approx.stop ~param;
                 call_function ~opening:false "destroy" approx.destroy ~param;
                 get_value "getAppletInfo" approx.applet_info `String ;
                 get_value "getParameterInfo" approx.parameter_info `StringArray2 ]
               @ (match !Jclflags.applet with
               | Some Jclflags.Graphics ->
                   [call_function ~opening:true "run" approx.run ~param:`Event]
               | Some (Jclflags.Awt | Jclflags.Swing) | None ->
                   []))
     ~attributes:[])
