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


(** OCaml-Java-specific command-line parameters. *)

type applet_kind =
  | Graphics
      (** The applet is based on the {i Graphics} module. *)
  | Awt
      (** The applet is based on the {i java.awt.Applet} class. *)
  | Swing
      (** The applet is based on the {i javax.swing.JApplet} class. *)
(** The type of applet kind, specifying its GUI. *)

type servlet_kind =
  | Generic
      (** The class extends {i javax.servlet.GenericServlet}. *)
  | Http
      (** The class extends {i javax.servlet.http.HttpServlet}. *)
  | Filter
      (** The class extends {i javax.servlet.Filter}. *)
  | Context_listener
      (** The class extends {i javax.servlet.ServletContextListener}. *)
  | Context_attribute_listener
      (** The class extends {i javax.servlet.ServletContextAttributeListener}. *)
  | Session_listener
      (** The class extends {i javax.servlet.http.HttpSessionListener}. *)
  | Session_activation_listener
      (** The class extends {i javax.servlet.http.HttpSessionActivationListener}. *)
  | Session_attribute_listener
      (** The class extends {i javax.servlet.http.HttpSessionAttributeListener}. *)
  | Session_binding_listener
      (** The class extends {i javax.servlet.http.HttpSessionBindingListener}. *)
  | Session_id_listener
      (** The class extends {i javax.servlet.http.HttpSessionIdListener}. *)
(** The type of servlet (or filter/listener) kind. *)

val additional_classes : string list ref
(** Additional classes to add to produced jar file. *)

val additional_files : string list ref
(** Additional files to add to produced jar file. *)

val additional_jars : string list ref
(** Additional jar files to add to produced jar file. *)

val additional_jar_refs : string list ref
(** Additional jar files to add to classpath of produced jar file. *)

val applet : applet_kind option ref
(** Kind of applet to produce, [None] if not linking as an applet. *)

val classpath : string list ref
(** Classpath elements. *)

val classpath_reset : bool ref
(** Whether the classpath was reset ({i i.e.} does not include system
    classpath). *)

val dump_bytecode : bool ref
(** Whether to dump the bytecode for compiled functions. *)

val dump_jlambda : bool ref
(** Whether to dump the jlambda for compiled functions. *)

val dump_minstr : bool ref
(** Whether to dump the macroinstruction for compiled functions. *)

val dump_optbytecode : bool ref
(** Whether to dump the optimized bytecode for compiled functions. *)

val dump_primitives : bool ref
(** Whether to dump the list of primitives. *)

val java_extensions : bool ref
(** Whether to enable Java extensions. *)

val java_generics : bool ref
(** Whether to enable Java generics. *)

val java_internal_types : bool ref
(** Whether to display internal encoding of Java types. *)

val java_package : string ref
(** Package for produced class files. *)

val javafx_application : bool ref
(** Whether to link as a JavaFX application. *)

val nobuiltin : bool ref
(** Whether to ignore builtin primitives. *)

val nomerge : bool ref
(** Whether to fail if a service is defined more than once. *)

val opt_floats : bool ref
(** Whether to optimize float operations. *)

val opt_unroll_loops : bool ref
(** Whether to enable unrolling of loops. *)

val providers : string list ref
(** List of additional primitive providers. *)

val runtime_parameters : string list ref
(** List of runtime parameters to be stored in produced jar file. *)

val scripting : bool ref
(** Whether to compile for use by scripting engine. *)

val servlet : servlet_kind option ref
(** Kind of servlet to produce, [None] if not compiling as a servlet. *)

val signals : bool ref
(** Whether to generate code checking signals. *)

val standalone : bool ref
(** Whether to produce a standalone jar file. *)

val war : string option ref
(** Whether to produce a war file, along with descriptor file. *)
