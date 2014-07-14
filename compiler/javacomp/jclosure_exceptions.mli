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

type exception_tag =
  | Predefined_exception of Ident.t
      (** Represents the predefined exception whose identifier is given. *)
  | Defined_exception of Ident.t * int
      (** Represents the user-defined exception whose module and index are given. *)
  | Any_exception
      (** Represents the [_] pattern over exceptions. *)
(** The type of exceptions tags ({i i.e.} identifiers). *)

val same_exception_tag : exception_tag -> exception_tag -> bool
(** Equality over exception tags. *)

val extract_try_with_handlers : Jlambda.jlambda -> (exception_tag * Jlambda.jlambda) list
(** [extract_try_with_handlers handler] returns a list of [tag, handl]
    couples that represent the clauses [with e0 -> h0 | ... | en -> hn]
    for [handler] (that should appear in a [try ... with handler]
    lambda. *)

val extract_raised : Jlambda.jlambda -> exception_tag list
(** [extract_raised body] returns a list if tags that represent the
    exceptions that can be raised from [body] (that should appear in a
    [try body with ...] lambda. *)

val replace_raise : (exception_tag * Jlambda.jlambda * bool * int) list -> Jlambda.jlambda -> Jlambda.jlambda
(** [replace_raise handlers body] replace [raise]s in [body] with
    staticfails ({i i.e.} jumps to handlers). Each element of [handlers]
    specifies:
    - the exception actually treated by the handler;
    - the code associated with the handler;
    - whether the exception instance is used by the handler;
    - the identifier of the staticfail/staticcatch couple. *)
