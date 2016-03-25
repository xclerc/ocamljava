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


type applet_kind =
  | Graphics
  | Awt
  | Swing

type servlet_kind =
  | Generic
  | Http
  | Filter
  | Context_listener
  | Context_attribute_listener
  | Session_listener
  | Session_activation_listener
  | Session_attribute_listener
  | Session_binding_listener
  | Session_id_listener

let additional_classes = ref []       (* -additional-class ... *)

let additional_files = ref []         (* -additional-file ... *)

let additional_jars = ref []          (* -additional-jar ... *)

let additional_jar_refs = ref []      (* -additional-jar-ref ... *)

let applet = ref None                 (* -applet ... *)

let classpath = ref []                (* -classpath ... / -cp ... *)

let classpath_reset = ref false       (* -classpath ... / -cp ... *)

let dump_bytecode = ref false         (* -dbytecode *)

let dump_jlambda = ref false          (* -djlambda *)

let dump_minstr = ref false           (* -dminstr *)

let dump_optbytecode = ref false      (* -doptbytecode *)

let dump_primitives = ref false       (* -dprimitives *)

let java_extensions = ref false       (* -java-extensions *)

let java_generics = ref false         (* -java-generics *)

let java_internal_types = ref false   (* -java-internal-types *)

let java_package = ref "pack"         (* -java-package ... *)

let javafx_application = ref false    (* -javafx-application *)

let nobuiltin = ref false             (* -nobuiltin *)

let nomerge = ref false               (* -nomerge *)

let opt_floats = ref false            (* -opt-floats *)

let opt_unroll_loops = ref false      (* -opt-unroll-loops *)

let providers = ref []                (* -provider ... *)

let runtime_parameters = ref []       (* -runtime-parameter ... *)

let scripting = ref false             (* -scripting *)

let servlet = ref None                (* -servlet ... *)

let signals = ref false               (* -signals *)

let standalone = ref true             (* -standalone / -shared-libraries *)

let war = ref None                    (* -war ... *)
