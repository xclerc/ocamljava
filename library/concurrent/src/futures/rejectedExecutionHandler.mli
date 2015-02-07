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

(** Policies for blocked executions. *)


type t =
  | Abort_policy (** Reject computation. *)
  | Caller_runs_policy (** Run the computation in the calling thread. *)
  | Discard_oldest_policy (** Discard the oldest unstarted computation. *)
  | Discard_policy (** Discard the submitted computation. *)
(** The type of policies for blocked executions, that is when thread and
    queue bounds have been reached. *)
