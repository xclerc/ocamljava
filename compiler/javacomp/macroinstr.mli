(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Macroinstructions. *)

type value_kind =
  | Boxed_value
        (** A boxed value. *)
  | Tagged_int
        (** An unboxed 63-bit integer (for type [int]), in tagged representation. *)
  | Normalized_int
        (** An unboxed 63-bit integer (for type [int]), untagged but normalized. *)
  | Unboxed_int
        (** An unboxed 64-bit integer (for type [int]). *)
  | Unboxed_int32
        (** An unboxed 32-bit integer (for type [int32]). *)
  | Unboxed_int64
        (** An unboxed 64-bit integer (for type [int64]). *)
  | Unboxed_nativeint
        (** An unboxed 64-bit integer (for type [nativeint]). *)
  | Unboxed_float
        (** An unboxed float. *)
  | Unboxed_instance of string
        (** An unboxed Java instance, with class name. *)
  | Unboxed_java_array of Jlambda.array_type
        (** An unboxed Java array, with exact type. *)
(** The type of runtime representation for values. *)

val int_kind : value_kind
(** The kind to be used for values of type [int], depending on
    configuration-time options. *)

val array_type_of_repr : Lambda.repr -> Jlambda.array_type
(** Converts the passed representation (assumed to describe an array) to
    its type equivalent. *)

val kind_of_repr : Lambda.repr -> value_kind
(** Converts the passed representation into a value kind. *)

val size_of_value_kind : value_kind -> int
(** Returns the size of the passed value kind, in terms of {i locals}
    slots. *)

type parameters =
  | Fixed of value_kind list
        (** Fixed number of parameters (regular primitives). *)
  | Unbounded of value_kind
        (** Unbounded number of parameters
            (special primitives - {i e.g.} makeblock, makearray). *)
(** The type of primitive signatures. *)

val kind_of_boxed_integer : Lambda.boxed_integer -> value_kind
(** [kind_of_boxed_integer bi] returns the kind to be used to store a
    value of type [bi]. *)

val combine_kinds : value_kind -> value_kind -> value_kind
(** [combine_kinds k1 k2] returns the kind to be used to store a value
    that can be either of kind [k1] or kind [k2]. *)

val repeat_kind : value_kind -> int -> value_kind list
(** [repeat_kind k n] returns a list of [n] elements all equal to [k]. *)

type expression =
  | Mconst of Jlambda.const
  | Moffset of expression * int
  | Mdynamicoffset of expression * expression
  | Mcreateglobal of string * string
  | Mclosure of int * mfunction list * expression list
  | Mreadlocal of value_kind * int
  | Mwritelocal of value_kind * int * expression
  | Mpoptolocal of value_kind * int
  | Mcall of Jlambda.function_label * expression list * value_kind list *  value_kind option * Debuginfo.t
  | Mprim of Lambda.primitive * expression list * Debuginfo.t
  | Mjavaprim of Jlambda.java_primitive * expression list * Debuginfo.t
  | Mapply of int * expression list * Debuginfo.t
  | Msend of int * int * int * expression list * Debuginfo.t
  | Msequence of expression * expression
  | Mifthenelse of expression * expression * expression
  | Mwhile of expression * expression * Jlambda.jlamdba_loop_inlining_info
  | Mfor of int * Asttypes.direction_flag * int * expression * Jlambda.jlamdba_loop_inlining_info
  | Munit
  | Mboxedunit
  | Mpop of value_kind
  | Mnop
  | Mtrywith of expression * (int option) * expression
  | Mstaticfail of int * expression list
  | Mstaticcatch of int * expression * expression
  | Mswitch of expression * int array * int array * expression array * expression
  | Mconvert of value_kind * value_kind * expression
  | Minit of int option
and mfunction = {
    mfun_label : Jlambda.function_label;
    mfun_ocaml_arity : int;
    mfun_java_arity : int;
  }

type fundecl =
    { fun_name : string;
      fun_args : Ident.t list;
      fun_params : Lambda.repr list;
      fun_return : Lambda.repr;
      fun_body : expression; }
