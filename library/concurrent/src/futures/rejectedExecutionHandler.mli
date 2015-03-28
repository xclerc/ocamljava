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


type t = java'util'concurrent'RejectedExecutionHandler java_instance
(** The type of policies for blocked executions, that is when thread and
    queue bounds have been reached. *)

val abort_policy : t
(** Reject computation; see
    {java java.util.concurrent.ThreadPoolExecutor.AbortPolicy}. *)

val caller_runs_policy : t
(** Run the computation in the calling thread; see
    {java java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy}. *)

val discard_oldest_policy : t
(** Discard the oldest unstarted computation; see
    {java java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy}. *)

val discard_policy : t
(** Discard the submitted computation; see
    {java java.util.concurrent.ThreadPoolExecutor.DiscardPolicy}. *)
