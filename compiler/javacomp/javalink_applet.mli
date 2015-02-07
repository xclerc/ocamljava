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

val check_applet_signature : Cmj_format.unit_infos -> string -> Types.signature * Jlambda.value_approximation array
(** [check_applet_signature unit_infos cmj_file] checks that the module
    defined by [unit_infos] and stored in [cmj_file] has a correct
    signature with respect to the kind of applet to compile.

    Returns the actual type for the applet, and the approximation for
    its module implementation. *)

type approx = {
    applet_info : Jlambda.value_approximation * int;
    parameter_info : Jlambda.value_approximation * int;
    init : Jlambda.value_approximation * int;
    start : Jlambda.value_approximation * int;
    run : Jlambda.value_approximation * int;
    stop : Jlambda.value_approximation * int;
    destroy : Jlambda.value_approximation * int;
  }
(** Approximation for applet: approximation value, and approximation
    index for each signature element. *)

val extract_applet_approx : Types.signature_item list -> Jlambda.value_approximation list -> approx
(** Returns the applet-specific approximation for the passed signature
    and full approximation corresponding to the applet module. *)

val make_applet_class : Cmj_format.unit_infos -> approx -> string * BaristaLibrary.Bytes.t
(** Compiles the main applet class, and returns its name and class file
    contents. *)
