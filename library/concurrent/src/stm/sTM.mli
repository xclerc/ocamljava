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

(** Minimalistic implementation of a Software Transactional Memory.

    {b WARNING:} the current implementation has only been lightly tested.

    This module provides support for a {i partial} STM, that protects
    only values of type [STM.ref]. *)

type 'a ref
(** The type of values protected by transactions, should store only
    immutable values. *)

val ref : 'a -> 'a ref
(** Builds a value of type [ref], that can then only be accessed during
    a transaction. *)

exception Retry
(** The exception used to request the current transaction to restart. *)

exception Abort
(** The exception used to request the current transaction to be
    cancelled. Any exception can be used to abort a transaction, but this
    one makes programs more readable. *)

exception Cancelled
(** The exception used to indicate that the transaction is cancelled,
    because it has failed and retries have been exhausted. *)

val run : ?retries:int -> (('a ref -> 'a) -> ('a ref -> 'a -> unit) -> 'b) -> 'b
(** [run ~retries f] executes the function [f] inside a newly-created
    transaction. Function [f] is passed two functions [g] and [s] that
    are to be used as respectively read and write accessors to [ref]
    values. The [retries] parameter (defaulting to [64]) indicates how
    many time a transaction should be retried. [g] and [s] will raise
    [Failure] is they are called from another thread, or outside the
    lifetime  of the transaction.

    The [Retry] exception can be raised to requested the current
    transaction to be re-executed from the beginning, while any other
    exception will cause the transaction to be aborted.

    Raises [Cancelled] if the transaction cannot be committed, and
    retries have been exhausted.

    Raises the exception raised by the transaction, if different from
    [Retry]. *)

val run_read_only : ?retries:int -> (('a ref -> 'a) -> 'b) -> 'b
(** Akin to [run], but with a smaller overhead due to the fact that the
    transaction is guaranteed to only read values. *)
