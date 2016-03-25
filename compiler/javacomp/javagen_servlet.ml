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

type parameter =
  | Nothing
  | Impl_value
  | Impl_that
  | ServletRequest
  | ServletResponse
  | HttpServletRequest
  | HttpServletResponse
  | FilterConfig
  | FilterChain
  | ServletContextEvent
  | ServletContextAttributeEvent
  | HttpSessionEvent
  | HttpSessionBindingEvent
  | String
  | Unit_value

let class_of_parameter = function
  | Nothing -> raise Not_found
  | Impl_value -> raise Not_found
  | Impl_that -> raise Not_found
  | ServletRequest -> "javax.servlet.ServletRequest"
  | ServletResponse -> "javax.servlet.ServletResponse"
  | HttpServletRequest -> "javax.servlet.http.HttpServletRequest"
  | HttpServletResponse -> "javax.servlet.http.HttpServletResponse"
  | FilterConfig -> "javax.servlet.FilterConfig"
  | FilterChain -> "javax.servlet.FilterChain"
  | ServletContextEvent -> "javax.servlet.ServletContextEvent"
  | ServletContextAttributeEvent -> "javax.servlet.ServletContextAttributeEvent"
  | HttpSessionEvent -> "javax.servlet.http.HttpSessionEvent"
  | HttpSessionBindingEvent -> "javax.servlet.http.HttpSessionBindingEvent"
  | String -> "java.lang.String"
  | Unit_value -> raise Not_found

type return =
  | Value
  | Unit
  | Int64

type infos = {
    parent : string;
    interface : bool;
    filter_or_listener : bool;
    functions : (string * (parameter list) * return * string) list;
  }

let infos_of_kind = function
  | Jclflags.Generic ->
      { parent    = "javax.servlet.GenericServlet";
        interface = false;
        filter_or_listener  = false;
        functions = [ "init",
                      [Nothing],
                      Value,
                      "init" ;
                      "service",
                      [Impl_value; Impl_that; ServletRequest; ServletResponse],
                      Unit,
                      "service" ;
                      "destroy",
                      [Impl_value; Impl_that],
                      Unit,
                      "destroy" ]; }
  | Jclflags.Http ->
      { parent    = "javax.servlet.http.HttpServlet";
        interface = false;
        filter_or_listener  = false;
        functions = [ "init",
                      [Nothing],
                      Value,
                      "init" ;
                      "do_delete",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doDelete" ;
                      "do_get",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doGet" ;
                      "do_head",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doHead" ;
                      "do_options",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doOptions" ;
                      "do_post",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doPost" ;
                      "do_put",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doPut" ;
                      "do_trace",
                      [Impl_value; Impl_that; HttpServletRequest; HttpServletResponse],
                      Unit,
                      "doTrace" ;
                      "get_last_modified",
                      [Impl_value; Impl_that; HttpServletRequest],
                      Int64,
                      "getLastModified" ;
                      "destroy",
                      [Impl_value; Impl_that],
                      Unit,
                      "destroy" ]; }
  | Jclflags.Filter ->
      { parent    = "javax.servlet.Filter";
        interface = true;
        filter_or_listener = true;
        functions = [ "init",
                      [FilterConfig],
                      Unit,
                      "init" ;
                      "do_filter",
                      [ServletRequest; ServletResponse; FilterChain],
                      Unit,
                      "doFilter" ;
                      "destroy",
                      [Unit_value],
                      Unit,
                      "destroy" ]; }
  | Jclflags.Context_listener ->
      { parent    = "javax.servlet.ServletContextListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "context_initialized",
                      [ServletContextEvent],
                      Unit,
                      "contextInitialized" ;
                      "context_destroyed",
                      [ServletContextEvent],
                      Unit,
                      "contextDestroyed" ]; }
  | Jclflags.Context_attribute_listener ->
      { parent    = "javax.servlet.ServletContextAttributeListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "attribute_added",
                      [ServletContextAttributeEvent],
                      Unit,
                      "attributeAdded" ;
                      "attribute_removed",
                      [ServletContextAttributeEvent],
                      Unit,
                      "attributeRemoved" ;
                      "attribute_replaced",
                      [ServletContextAttributeEvent],
                      Unit,
                      "attributeReplaced" ]; }
  | Jclflags.Session_listener ->
      { parent    = "javax.servlet.http.HttpSessionListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "session_created",
                      [HttpSessionEvent],
                      Unit,
                      "sessionCreated" ;
                      "session_destroyed",
                      [HttpSessionEvent],
                      Unit,
                      "sessionDestroyed" ]; }
  | Jclflags.Session_activation_listener ->
      { parent    = "javax.servlet.http.HttpSessionActivationListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "session_did_activate",
                      [HttpSessionEvent],
                      Unit,
                      "sessionDidActivate" ;
                      "session_will_passivate",
                      [HttpSessionEvent],
                      Unit,
                      "sessionWillPassivate" ]; }
  | Jclflags.Session_attribute_listener ->
      { parent    = "javax.servlet.http.HttpSessionAttributeListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "attribute_added",
                      [HttpSessionBindingEvent],
                      Unit,
                      "attributeAdded" ;
                      "attribute_removed",
                      [HttpSessionBindingEvent],
                      Unit,
                      "attributeRemoved" ;
                      "attribute_replaced",
                      [HttpSessionBindingEvent],
                      Unit,
                      "attributeReplaced" ]; }
  | Jclflags.Session_binding_listener ->
      { parent    = "javax.servlet.http.HttpSessionBindingListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "value_bound",
                      [HttpSessionBindingEvent],
                      Unit,
                      "valueBound" ;
                      "value_unbound",
                      [HttpSessionBindingEvent],
                      Unit,
                      "valueUnbound" ]; }
  | Jclflags.Session_id_listener ->
      { parent    = "javax.servlet.http.HttpSessionIdListener";
        interface = true;
        filter_or_listener  = true;
        functions = [ "session_id_changed",
                      [HttpSessionEvent; String],
                      Unit,
                      "sessionIdChanged" ]; }

let return_bytes cd =
  let buff = ByteBuffer.make_of_size 1024 in
  let stream = OutputStream.make_of_buffer buff in
  ClassFile.write (ClassDefinition.encode cd) stream;
  OutputStream.close stream;
  ByteBuffer.contents buff

let node = Instrtree.node

let leaf = Instrtree.leaf

let flatten = Instrtree.flatten

let extract_func_name func_name signature approx =
  let rec efn approx idx = function
    | Types.Sig_value (path, _) :: tl ->
        let name = Ident.name path in
        let curr, approx = match approx with hd :: tl -> hd, tl | [] -> assert false in
        if name = func_name then
          curr, idx
        else
          efn approx (succ idx) tl
    | Types.Sig_exception _ :: tl | Types.Sig_module _ :: tl | Types.Sig_class _ :: tl ->
        let approx = match approx with _ :: tl -> tl | [] -> assert false in
        efn approx (succ idx) tl
    | Types.Sig_type _ :: tl | Types.Sig_modtype _ :: tl | Types.Sig_class_type _ :: tl ->
        efn approx idx tl
    | [] -> print_endline func_name; assert false in
  match efn approx 0 signature with
  | Jlambda.Value_closure ({ Jlambda.fun_label = { Jlambda.fl_class; fl_method };
                             fun_closed;
                             _ }, _), idx ->
    fl_class, fl_method, idx, fun_closed
  | _ -> assert false

let push_env clas idx =
  leaf
    [ Instruction.INVOKESTATIC (make_class clas,
                                make_method "getGlobal",
                                ([], `Class (make_class (clas ^ "$Global")))) ;
      Instruction.LDC2_W (`Long (Int64.of_int idx)) ;
      Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                 make_method "get",
                                 ([`Long], `Class class_Value)) ]

let compile_methods impl_class_name infos signature approx =
  let impl_class_name = make_class impl_class_name in
  let ensure_init =
    Instruction.INVOKESTATIC (make_class (!Jclflags.java_package  ^ "." ^ Jconfig.main_servlet_class),
                              make_method"init",
                              ([], `Void)) in
  let constructor_code, max_stack =
    if infos.filter_or_listener then
      leaf [ Instruction.ALOAD_0 ;
             Instruction.INVOKESPECIAL (make_class (if infos.interface then "java.lang.Object" else infos.parent),
                                        make_method "<init>",
                                        ([], `Void)) ;
             ensure_init ;
             Instruction.RETURN ], 1
    else
      node
        [ leaf [ Instruction.ALOAD_0 ;
                 Instruction.INVOKESPECIAL (make_class infos.parent,
                                            make_method "<init>",
                                            ([], `Void));
                 Instruction.ALOAD_0 ;
                 Instruction.ACONST_NULL ;
                 Instruction.PUTFIELD (impl_class_name,
                                       make_field "value",
                                       `Class class_Value) ;
                 ensure_init ;
                 Instruction.ALOAD_0 ;
                 Instruction.DUP ] ;
          meth_createInstance ;
          leaf [ Instruction.PUTFIELD (impl_class_name,
                                       make_field "that",
                                       `Class class_Value) ;
                 Instruction.RETURN ] ], 2 in
  let constructor =
    Method.Constructor
      { Method.cstr_flags = [`Public];
        cstr_descriptor = [];
        cstr_attributes = [`Code { Attribute.max_stack = u2 max_stack;
                                   Attribute.max_locals = u2 1;
                                   Attribute.code = flatten constructor_code;
                                   Attribute.exception_table = [];
                                   Attribute.attributes = []; }]; } in
  let methods =
    List.map
      (fun (func_name, func_params, func_return, meth_name) ->
        let idx = ref 0 in
        let load_next_idx () =
          incr idx;
          aload !idx in
        let func_impl_class, fund_impl_meth, func_impl_idx, fun_impl_closed =
          extract_func_name func_name signature approx in
        let code =
          node
            [ node
                (List.map
                   (function
                     | Nothing | Impl_that ->
                         leaf [ Instruction.ALOAD_0 ;
                                Instruction.GETFIELD (impl_class_name,
                                                      make_field "that",
                                                      `Class class_Value) ]
                     | Impl_value ->
                         leaf [ Instruction.ALOAD_0 ;
                                Instruction.GETFIELD (impl_class_name,
                                                      make_field "value",
                                                      `Class class_Value) ]
                     | ServletRequest
                     | ServletResponse
                     | HttpServletRequest
                     | HttpServletResponse
                     | FilterConfig
                     | FilterChain
                     | ServletContextEvent
                     | ServletContextAttributeEvent
                     | HttpSessionEvent
                     | HttpSessionBindingEvent
                     | String ->
                         node [ load_next_idx () ;
                                meth_createInstance ]
                     | Unit_value ->
                         field_Value_UNIT)
                   func_params);
              (if fun_impl_closed then
                leaf []
              else
                push_env func_impl_class func_impl_idx) ;
              leaf [ Instruction.INVOKESTATIC (make_class func_impl_class,
                                               make_method fund_impl_meth,
                                               ((List.map (fun _ -> `Class class_Value) func_params)
                                                @ (if fun_impl_closed then [] else [`Class class_Value]),
                                                `Class class_Value)) ] ;
                     (match func_return with
                     | Value -> leaf [ Instruction.ALOAD_0 ;
                                       Instruction.SWAP ;
                                       Instruction.PUTFIELD (impl_class_name,
                                                             make_field "value",
                                                             `Class class_Value) ;
                                       Instruction.RETURN ]
                     | Unit  -> leaf [ Instruction.POP ;
                                       Instruction.RETURN ]
                     | Int64 -> node [ meth_asInt64 ;
                                       leaf [ Instruction.LRETURN ] ]) ] in
        let code = flatten code in
        let code_length = Instruction.size_of_list 0 code in
        let catch =
          [ Instruction.NEW (make_class "javax.servlet.ServletException") ;
            Instruction.DUP_X1 ;
            Instruction.SWAP ;
            Instruction.INVOKESPECIAL (make_class "javax.servlet.ServletException",
                                       make_method "<init>",
                                       ([`Class (make_class "java.lang.Throwable")], `Void)) ;
            Instruction.ATHROW ] in
        let exception_table = [ { Attribute.try_start = u2 0;
                                  try_end = u2 (pred code_length);
                                  catch = u2 code_length;
                                  caught = None } ] in
        let meth_params =
          List.fold_right
            (fun elem acc ->
              try
                (`Class (make_class (class_of_parameter elem))) :: acc
              with Not_found ->
                acc)
            func_params
            [] in
        let code, _, exception_table, graph =
          ControlFlow.graph_of_instructions (code @ catch) exception_table
          |> Code.flatten_graph ~use_offsets:true in
        let max_stack, max_locals, stack_map_frame =
          Code.compute_stack_infos
            impl_class_name
            (Jutils.get_unifier ())
            graph
            (StackState.make_of_parameters
               (Some (impl_class_name, false))
               meth_params) in
        let attributes =
          if stack_map_frame <> [] then
            [ `StackMapTable stack_map_frame ]
          else
            [] in
        Method.Regular
          { Method.flags = [ `Public ];
  	    Method.name = make_method meth_name;
  	    Method.descriptor = (meth_params,
                                 (match func_return with
                                 | Value -> `Void
                                 | Unit  -> `Void
                                 | Int64 -> `Long));
  	    Method.attributes = [`Code { Attribute.max_stack;
                                         Attribute.max_locals;
                                         Attribute.code;
                                         Attribute.exception_table;
                                         Attribute.attributes }]; })
      infos.functions in
  constructor :: methods

let compile kind mod_kind base_class_name signature =
  let approx =
    match (Jcompilenv.current_unit_infos ()).Cmj_format.ui_approx with
    | Jlambda.Value_tuple x -> x
    | _ -> failwith mod_kind in
  let impl_class_name = base_class_name ^ "Impl" in
  let infos = infos_of_kind kind in
  let parent_class, implemented_interfaces =
    if infos.interface then
      "java.lang.Object", [infos.parent]
    else
      infos.parent, [] in
  let fields =
    if infos.filter_or_listener then
      []
    else
      [ { Field.flags = [`Private];
          name = make_field "value";
          descriptor = `Class class_Value;
          attributes = []; } ;
        { Field.flags = [`Private; `Final];
          name = make_field "that";
          descriptor = `Class class_Value;
          attributes = []; }] in
  let methods = compile_methods impl_class_name infos signature (Array.to_list approx) in
  return_bytes { ClassDefinition.access_flags = [`Public; `Final; `Super];
                 name = make_class impl_class_name;
                 extends = Some (make_class parent_class) ;
                 implements = List.map make_class implemented_interfaces;
                 fields;
                 methods;
                 attributes = []; }
