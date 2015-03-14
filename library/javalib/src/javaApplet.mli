(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Support for Java applets.

    In order to produce a jar archive containing an applet, it is
    necessary to link the application with the {k -applet {i k}}
    command-line switch, where {k {i k}} designates the kind of applet
    (AWT-, Swing-, or Graphics-based). An applet class will be generated,
    with the name {i pack.ocamljavaApplet} where {i pack} can be set
    using the {k -java-package {i pack}} command-line switch.

    When linking with the {k -applet {i k}} command-line switch, the last
    module to be linked has to be compatible with one of the module types
    of the [JavaApplet] module, the exact module type depending on the
    value of {k {i k}}. The following table gives the module types and
    default implementations (modules compatibles with the module types,
    with only no-op functions) for the different applet kinds.

    {C {table {caption The various kinds of applets.}
              {row {header parameter to {k -applet}}
                   {header module type}
                   {header default implementation}}
              {row {data {k awt}}
                   {data [AWT]}
                   {data [Default_AWT]}}
              {row {data {k swing}}
                   {data [Swing]}
                   {data [Default_Swing]}}
              {row {data {k graphics}}
                   {data [Graphics]}
                   {data [Default_Graphics]}}}} *)


(** {6 Applet information} *)

type parameter = {
    param_name : JavaString.t; (** parameter name. *)
    param_type : JavaString.t; (** parameter type. *)
    param_desc : JavaString.t; (** parameter description. *)
  }
(** The type of parameters, describing parameters accepted by an applet. *)

type parameter_info
(** The type of parameter info, as returned by the {java java.applet.Applet#getParameterInfo()}
    method. *)

val parameter_info_of_list : parameter list -> parameter_info
(** Converts a list of parameters into a parameter info, preserving the
    order of elements. *)


(** {6 AWT-based applets} *)

type awt = java'applet'Applet java_instance
(** Shorthand for the type of AWT applets. *)

module type AWT = sig
  val applet_info : JavaString.t
  (** The value to be returned by the {java java.applet.Applet#getAppletInfo()} method. *)
  val parameter_info : parameter_info
  (** The value to be returned by the {java java.applet.Applet#getParameterInfo()} method. *)
  val init : awt -> unit
  (** The implementation of the {java java.applet.Applet#init()} method. *)
  val start : awt -> unit
  (** The implementation of the {java java.applet.Applet#start()} method. *)
  val stop : awt -> unit
  (** The implementation of the {java java.applet.Applet#stop()} method. *)
  val destroy : awt -> unit
  (** The implementation of the {java java.applet.Applet#destroy()} method. *)
end
(** The module type for applets linked with {k -applet awt}. *)

module Default_AWT : AWT
(** Default implementation for AWT applets. *)


(** {6 Swing-based applets} *)

type swing = javax'swing'JApplet java_instance
(** Shorthand for the type of Swing applets. *)

module type Swing = sig
  val applet_info : JavaString.t
  (** The value to be returned by the {java java.applet.Applet#getAppletInfo()} method. *)
  val parameter_info : parameter_info
  (** The value to be returned by the {java java.applet.Applet#getParameterInfo()} method. *)
  val init : swing -> unit
  (** The implementation of the {java java.applet.Applet#init()} method. *)
  val start : swing -> unit
  (** The implementation of the {java java.applet.Applet#start()} method. *)
  val stop : swing -> unit
  (** The implementation of the {java java.applet.Applet#stop()} method. *)
  val destroy : swing -> unit
  (** The implementation of the {java java.applet.Applet#destroy()} method. *)
end
(** The module type for applets linked with {k -applet swing}. *)

module Default_Swing : Swing
(** Default implementation for Swing applets. *)


(** {6 Graphics-based applets} *)

type graphics_event = {
  mouse_x    : int;  (** X coordinate of the mouse. *)
  mouse_y    : int;  (** Y coordinate of the mouse. *)
  button     : bool; (** [true] if a mouse button is pressed. *)
  keypressed : bool; (** [true] if a key has been pressed. *)
  key        : char; (** the character for the key pressed. *)
}
(** Equivalent to {!Graphics.status}, copied to avoid dependency. *)

module type Graphics = sig
  val applet_info : JavaString.t
  (** The value to be returned by the {java java.applet.Applet#getAppletInfo()} method. *)
  val parameter_info : parameter_info
  (** The value to be returned by the {java java.applet.Applet#getParameterInfo()} method. *)
  val init : unit -> unit
  (** The implementation of the {java java.applet.Applet#init()} method. *)
  val start : unit -> unit
  (** The implementation of the {java java.applet.Applet#start()} method. *)
  val run : graphics_event -> unit
  (** Callback called for each event. *)
  val stop : unit -> unit
  (** The implementation of the {java java.applet.Applet#stop()} method. *)
  val destroy : unit -> unit
  (** The implementation of the {java java.applet.Applet#destroy()} method. *)
end
(** The module type for applets linked with {k -applet graphics}. *)

module Default_Graphics : Graphics
(** Default implementation for Graphics applets. *)
