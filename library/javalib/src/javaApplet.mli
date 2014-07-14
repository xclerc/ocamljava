(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2014 Xavier Clerc.
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
    necessary to link the application with the {i -applet k} command-line
    switch, where {i k} designate the kind of applet (AWT-, Swing-, or
    Graphics-based). An applet class will be generated, with name
    {i pack.ocamljavaApplet} where {i pack} can be set using the
    {i -java-package p} command-line switch.

    When linking with the {i -applet k} command-line switch, the last
    module to be linked has to abide to one of the module types of the
    [JavaApplet] module, the exact module type depending on the value of
    {i k}. *)


(** {6 Applet information} *)

type parameter = {
    param_name : java'lang'String java_instance; (** parameter name. *)
    param_type : java'lang'String java_instance; (** parameter type. *)
    param_desc : java'lang'String java_instance; (** parameter description. *)
  }
(** The type of parameters, describing parameters accepted by an applet. *)

type parameter_info
(** The type of parameter info, as returned by the {i getParameterInfo()}
    method. *)

val parameter_info_of_list : parameter list -> parameter_info
(** Converts a list of parameters into a parameter info, preserving the
    order of elements. *)


(** {6 AWT-based applets} *)

type awt = java'applet'Applet java_instance
(** Shorthand for the type of AWT applets. *)

module type AWT = sig
  val applet_info : java'lang'String java_instance
  (** The value to be returned by the {i getAppletInfo()} method. *)
  val parameter_info : parameter_info
  (** The value to be returned by the {i getParameterInfo()} method. *)
  val init : awt -> unit
  (** The implementation of the {i init()} method. *)
  val start : awt -> unit
  (** The implementation of the {i start()} method. *)
  val stop : awt -> unit
  (** The implementation of the {i stop()} method. *)
  val destroy : awt -> unit
  (** The implementation of the {i destroy()} method. *)
end
(** The module type for applets linked with {i -applet awt}. *)

module Default_AWT : AWT
(** Default ({i i.e.} empty) implementation for AWT applets. *)


(** {6 Swing-based applets} *)

type swing = javax'swing'JApplet java_instance
(** Shorthand for the type of Swing applets. *)

module type Swing = sig
  val applet_info : java'lang'String java_instance
  (** The value to be returned by the {i getAppletInfo()} method. *)
  val parameter_info : parameter_info
  (** The value to be returned by the {i getParameterInfo()} method. *)
  val init : swing -> unit
  (** The implementation of the {i init()} method. *)
  val start : swing -> unit
  (** The implementation of the {i start()} method. *)
  val stop : swing -> unit
  (** The implementation of the {i stop()} method. *)
  val destroy : swing -> unit
  (** The implementation of the {i destroy()} method. *)
end
(** The module type for applets linked with {i -applet swing}. *)

module Default_Swing : Swing
(** Default ({i i.e.} empty) implementation for Swing applets. *)


(** {6 Graphics-based applets} *)

type graphics_event = {
  mouse_x : int; (** X coordinate of the mouse. *)
  mouse_y : int; (** Y coordinate of the mouse. *)
  button : bool; (** [true] if a mouse button is pressed. *)
  keypressed : bool; (** [true] if a key has been pressed. *)
  key : char; (** the character for the key pressed. *)
}
(** Equivalent to [Graphics.status], copied to avoid dependency. *)

module type Graphics = sig
  val applet_info : java'lang'String java_instance
  (** The value to be returned by the {i getAppletInfo()} method. *)
  val parameter_info : parameter_info
  (** The value to be returned by the {i getParameterInfo()} method. *)
  val init : unit -> unit
  (** The implementation of the {i init()} method. *)
  val start : unit -> unit
  (** The implementation of the {i start()} method. *)
  val run : graphics_event -> unit
  (** Callback called for each event. *)
  val stop : unit -> unit
  (** The implementation of the {i stop()} method. *)
  val destroy : unit -> unit
  (** The implementation of the {i destroy()} method. *)
end
(** The module type for applets linked with {i -applet graphics}. *)

module Default_Graphics : Graphics
(** Default ({i i.e.} empty) implementation for Graphics applets. *)
