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

(** Thread-local variables. *)


type 'a t
(** The type of thread-local variables, providing an independent value to
    each thread. *)

external make : 'a -> 'a t =
  "ocamljava_threadlocal_make"
(** Returns a new thread-local variable whose initial value in each
    thread is the passed value. *)

external get : 'a t -> 'a =
  "ocamljava_threadlocal_get"
(** Returns the value of the thread-local variable for the current
    thread. *)

external remove : 'a t -> unit =
  "ocamljava_threadlocal_remove"
(** Removes the value of the thread-local variable for the current
    thread. The variable can still be [set] or [get]. If no [set] occurs
    between [remove] and [get], the variable is reinitialized with the
    value originally passed to [make]. *)

external set : 'a t -> 'a -> unit =
  "ocamljava_threadlocal_set"
(** Modifies the value of the thread-local variable for the current
    thread. *)
