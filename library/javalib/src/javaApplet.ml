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


(* Applet information *)

type parameter = {
    param_name : java'lang'String java_instance;
    param_type : java'lang'String java_instance;
    param_desc : java'lang'String java_instance;
  }

type parameter_info = java'lang'String java_instance java_reference_array java_reference_array

let parameter_info_of_list l =
  let len = List.length l in
  let res = Java.make_array "String[][]" (Int32.of_int len) 3l in
  List.iteri
    (fun i x ->
      let arr = JavaReferenceArray.get res (Int32.of_int i) in
      JavaReferenceArray.set arr 0l x.param_name;
      JavaReferenceArray.set arr 1l x.param_type;
      JavaReferenceArray.set arr 2l x.param_desc)
    l;
  res


(* AWT-based applets *)

type awt = java'applet'Applet java_instance

module type AWT = sig
  val applet_info : java'lang'String java_instance
  val parameter_info : parameter_info
  val init : java'applet'Applet java_instance -> unit
  val start : java'applet'Applet java_instance -> unit
  val stop : java'applet'Applet java_instance -> unit
  val destroy : java'applet'Applet java_instance -> unit
end

module Default_AWT : AWT = struct
  let applet_info = JavaString.of_string "AWT applet"
  let parameter_info = parameter_info_of_list []
  let init _ = ()
  let start _ = ()
  let stop _ = ()
  let destroy _ = ()
end


(* Swing-based applets *)

type swing = javax'swing'JApplet java_instance

module type Swing = sig
  val applet_info : java'lang'String java_instance
  val parameter_info : parameter_info
  val init : javax'swing'JApplet java_instance -> unit
  val start : javax'swing'JApplet java_instance -> unit
  val stop : javax'swing'JApplet java_instance -> unit
  val destroy : javax'swing'JApplet java_instance -> unit
end

module Default_Swing : Swing = struct
  let applet_info = JavaString.of_string "Swing applet"
  let parameter_info = parameter_info_of_list []
  let init _ = ()
  let start _ = ()
  let stop _ = ()
  let destroy _ = ()
end


(* Graphics-based applets *)

type graphics_event = {
  mouse_x : int;
  mouse_y : int;
  button : bool;
  keypressed : bool;
  key : char;
}

module type Graphics = sig
  val applet_info : java'lang'String java_instance
  val parameter_info : parameter_info
  val init : unit -> unit
  val start : unit -> unit
  val run : graphics_event -> unit
  val stop : unit -> unit
  val destroy : unit -> unit
end

module Default_Graphics : Graphics = struct
  let applet_info = JavaString.of_string "Graphics applet"
  let parameter_info = parameter_info_of_list []
  let init () = ()
  let start () = ()
  let run _ = ()
  let stop () = ()
  let destroy () = ()
end
