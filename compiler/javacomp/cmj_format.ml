(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 * Original file (asmcomp/cmj_format.ml in the OCaml source
 * distribution) is Copyright (C) INRIA.
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

type unit_infos =
  { mutable ui_name: string;
    mutable ui_javaname: string;
    mutable ui_additional_classes: string list;
    mutable ui_symbol: string;
    mutable ui_defines: string list;
    mutable ui_imports_cmi: (string * Digest.t) list;
    mutable ui_imports_cmj: (string * Digest.t) list;
    mutable ui_approx: Jlambda.value_approximation;
    mutable ui_force_link: bool;
    mutable ui_ensure_init: string list;
    ui_ints_are_63_bit_long: bool; }

type library_infos =
  { lib_units: (unit_infos * Digest.t) list;
    lib_ccobjs: string list;
    lib_ints_are_63_bit_long: bool; }

type dynunit = {
  dynu_name: string;
  dynu_crc: Digest.t;
  dynu_imports_cmi: (string * Digest.t) list;
  dynu_imports_cmj: (string * Digest.t) list;
  dynu_defines: string list;
}

type dynheader = {
  dynu_magic: string;
  dynu_units: dynunit list;
}
