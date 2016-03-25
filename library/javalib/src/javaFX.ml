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

(** Support for JavaFX applications.

    In order to produce a jar archive containing a JavaFX application, it
    is necessary to link the application with the {k -javafx-application}
    command-line switch. An application class will be generated,
    with the name {i pack.ocamljavaFXApplication} where {i pack} can be
    set using the {k -java-package {i pack}} command-line switch.

    When linking with the {k -javafx-application {i k}} command-line
    switch, the last module to be linked has to be compatible with the
    [JavaFX.Application] module type. *)


(* Applications *)

type application = javafx'application'Application java_instance

type stage = javafx'stage'Stage java_instance

module type Application = sig
  val init : application -> unit
  val start : application -> stage -> unit
  val stop : application -> unit
end
