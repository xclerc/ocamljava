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

(** Read/write locks. *)


type t
(** The type of read/write locks that are indeed peirs of associated
    locks where the read lock can be held simultaneously by any number of
    threads as long as the write lock is available, while the write lock
    can be help by at most one thread at a time. *)

external make_reentrant : bool -> t =
  "ocamljava_readwritelock_make_reentrant"
(** Returns a new read/write lock, whose read and write locks are
    reentrant, the parameter indicates whether a {i fair} ordering policy
    is requested. *)

external read_lock : t -> Lock.t =
  "ocamljava_readwritelock_read_lock"
(** Returns the read lock of the read/write lock. *)

external write_lock : t -> Lock.t =
  "ocamljava_readwritelock_write_lock"
(** Returns the write lock of the read/write lock. *)
