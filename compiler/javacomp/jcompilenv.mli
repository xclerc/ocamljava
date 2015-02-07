(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/compilenv.mli in the OCaml source
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

(** Compilation environments for compilation units. *)

val reset: ?packname:string -> string -> unit
        (* Reset the environment and record the name of the unit being
           compiled (arg).  Optional argument is [-for-pack] prefix. *)

val current_unit_infos: unit -> Cmj_format.unit_infos
        (* Return the infos for the unit being compiled *)

val register_toplevel_unit : unit -> unit
        (* Add the info from the current unit to the global info table *)

val current_unit_name: unit -> string
        (* Return the name of the unit being compiled *)

val current_class_name: unit -> string
        (* Return the class name of the unit being compiled *)

val current_global_class_name: unit -> string
        (* Return the class name holding globals for the unit being compiled *)

val make_symbol: ?unitname:string -> string option -> string
       (* [make_symbol ~unitname:u None] returns the Java symbol that
           corresponds to the compilation unit [u] (default: the current unit).
           [make_symbol ~unitname:u (Some id)] returns the Java symbol that
           corresponds to symbol [id] in the compilation unit [u]
           (or the current unit). *)

val symbol_for_global: Ident.t -> string
        (* Return the Java symbol that refers to the given global identifier *)
val class_for_global: Ident.t -> string
        (* Return the Java class that refers to the given global identifier *)

val global_approx: Ident.t -> Jlambda.value_approximation
        (* Return the approximation for the given global identifier *)
val global_approx_no_dep: Ident.t -> Jlambda.value_approximation
        (* Record the approximation of the unit being compiled, but does
           record it as a dependency. *)
val set_global_approx: Jlambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled *)
val get_global_approx: unit -> Jlambda.value_approximation
        (* Return the approximation of the unit being compiled *)
val record_global_approx_toplevel: unit -> unit
        (* Record the current approximation for the current toplevel phrase *)


val read_unit_info: string -> Cmj_format.unit_infos * Digest.t
        (* Read infos and MD5 from a [.cmj] file. *)
val write_unit_info: Cmj_format.unit_infos -> string -> unit
        (* Save the given infos in the given file *)
val save_unit_info: string -> unit
        (* Save the infos for the current unit in the given file *)
val cache_unit_info: Cmj_format.unit_infos -> unit
        (* Enter the given infos in the cache.  The infos will be
           honored by [symbol_for_global] and [global_approx]
           without looking at the corresponding .cmj file. *)

val cmj_not_found_crc: Digest.t
        (* Special digest used in the [ui_imports_cmx] list to signal
           that no [.cmx] file was found and used for the imported unit *)

val read_library_info: string -> Cmj_format.library_infos
(** Reads the library info for the passed filename. *)

val check_signature : string -> string -> string -> string -> Types.signature
(** [check_signature module_name cmi_file sign_mod sign_submod] checks
    that module [module_name] whose compiled interface file is [cmi_file]
    is compatible with the module type whose path is [sign_mod].[sign_submod],
    returning the module signature.

    Raises [Failure] if passed module is not compatible with module type. *)

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string
  | Cannot_determine_class of string

exception Error of error

val report_error: Format.formatter -> error -> unit
(** Pretty-prints an error. *)
