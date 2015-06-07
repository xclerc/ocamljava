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

(** Thread-local variables. *)


type 'a t = java'lang'ThreadLocal java_instance
(** The type of thread-local variables, providing an independent value to
    each thread. *)

external make : 'a -> 'a t =
  "ocamljava_threadlocal_make"
(** Returns a new thread-local variable whose initial value in each
    thread is the passed value; see
    {java java.lang.ThreadLocal#ThreadLocal()} and
    {java java.lang.ThreadLocal#initialValue()}. *)

external get : 'a t -> 'a =
  "ocamljava_threadlocal_get"
(** Returns the value of the thread-local variable for the current
    thread; see {java java.lang.ThreadLocal#get()}. *)

external remove : 'a t -> unit =
  "ocamljava_threadlocal_remove"
(** Removes the value of the thread-local variable for the current
    thread. The variable can still be [set] or [get]. If no [set] occurs
    between [remove] and [get], the variable is reinitialized with the
    value originally passed to [make]; see
    {java java.lang.ThreadLocal#remove()}. *)

external set : 'a t -> 'a -> unit =
  "ocamljava_threadlocal_set"
(** Modifies the value of the thread-local variable for the current
    thread; see {java java.lang.ThreadLocal#set(T)}. *)


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
