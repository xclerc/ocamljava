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

(** Value exchangers. *)

type t
(** The type of exchangers, allowing two threads to swap values. *)

external make : unit -> t =
  "ocamljava_exchanger_make"
(** Returns a new exchanger. *)

external exchange : t -> 'a -> 'a =
  "ocamljava_exchanger_exchange"
(** Waits for another thread to arrive at the same exchange point, and
    then swaps the values provided by the two threads.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external exchange_time : t -> 'a -> int64 -> TimeUnit.t -> 'a =
  "ocamljava_exchanger_exchange_time"
(** [exchange_time e x t u] is similar to [exchange e x], except that the
    current thread will at most wait for [t] (time value whose unit is
    [u]).

    Raises [Runtime.Interrupted] if the thread is interrupted.

    Raises [Runtime.Timeout] if time has elapsed with no exchange. *)
