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

type 'a t = java'util'concurrent'Exchanger java_instance
(** The type of exchangers, allowing two threads to swap values. *)

val make : unit -> 'a t
(** Returns a new exchanger; see
    {java java.util.concurrent.Exchanger#Exchanger()}. *)

val exchange : 'a t -> 'a -> 'a
(** Waits for another thread to arrive at the same exchange point, and
    then swaps the values provided by the two threads; see
    {java java.util.concurrent.Exchanger#exchange(V)}.

    @raise Java_exception if the thread is interrupted *)

val exchange_time : 'a t -> 'a -> java_long -> TimeUnit.t -> 'a
(** [exchange_time e x t u] is similar to [exchange e x], except that the
    current thread will at most wait for [t] (time value whose unit is
    [u]); see
    {java java.util.concurrent.Exchanger#exchange(V, long, java.util.concurrent.TimeUnit)}.

    @raise Java_exception if the thread is interrupted
    @raise Java_exception if time has elapsed with no exchange *)


(** {6 Null value} *)

val null : 'a t
(** The [null] value. *)

external is_null : 'a t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : 'a t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : 'a t -> 'a t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : 'a t option -> 'a t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
