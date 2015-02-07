(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/cmj_format.mli in the OCaml source
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

(** Format of .cmj, .cmja and .cmjs files. *)

(* Each .jo file has a matching .cmj file that provides the following infos
   on the compilation unit:
     - list of other units imported, with MD5s of their .cmj files
     - approximation of the structure implemented
       (includes descriptions of known functions: arity and direct entry
        points)
     - Java class name
   The .cmj file contains these infos (as an externed record) plus a MD5
   of these infos *)

type unit_infos =
  { mutable ui_name: string;                          (* Name of unit implemented *)
    mutable ui_javaname: string;                      (* Java class name *)
    mutable ui_additional_classes: string list;       (* Additional Java classes *)
    mutable ui_symbol: string;                        (* Prefix for symbols *)
    mutable ui_defines: string list;                  (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmj: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: Jlambda.value_approximation;   (* Approx of the structure*)
    mutable ui_force_link: bool;                      (* Always linked *)
    mutable ui_ensure_init: string list;              (* Packages needing auto-initialization *)
    ui_ints_are_63_bit_long: bool; }                  (* Size of int values *)

(* Each .ja library has a matching .cmja file that provides the following
   infos on the library: *)

type library_infos =
  { lib_units: (unit_infos * Digest.t) list;
    lib_ccobjs: string list;
    lib_ints_are_63_bit_long: bool; }

(* Each .cmjs dynamically-loaded plugin contains an entry
   "PluginHeader" containing the following info
   (as an externed record) *)

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
