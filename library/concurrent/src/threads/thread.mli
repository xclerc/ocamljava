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

(** Threads. *)


type state =
  | New (** The thread has not been started. *)
  | Runnable (** The thread is executing. *)
  | Blocked (** The thread is blocked on a lock. *)
  | Waiting (** The thread is waiting for another thread (with no timeout). *)
  | Timed_waiting (** The thread is waiting for another thread (with a timeout). *)
  | Terminated (** The thread has terminated execution. *)
(** The type of thread states. *)

val max_priority : int32
(** The maximum priority for a thread. *)

val min_priority : int32
(** The minimum priority for a thread. *)

val norm_priority : int32
(** The normal ({i i. e. default}) priority for a thread. *)

type t
(** The type of threads. If a thread raises an exception escapes, it is
    written to the standard output and then discarded. *)

external make : ThreadGroup.t option -> string option -> ('a -> unit) -> 'a -> t =
  "ocamljava_thread_make"
(** [make g n f x] returns a new thread in group [g] with name [n]. The
    thread will execute [f x] when started. *)

external current_thread : unit -> t =
  "ocamljava_thread_current_thread"
(** Returns the thread that is currently executing. *)

external get_id : t -> int64 =
  "ocamljava_thread_get_id"
(** Returns the thread identifier. The identifier is positive, and
    guaranteed to be unique while the thread is executing. The identifier
    can be recycled when the thread has terminated its execution. *)

external get_name : t -> string =
  "ocamljava_thread_get_name"
(** Returns the name of the thread. *)

external get_priority : t -> string =
  "ocamljava_thread_get_priority"
(** Returns the priority of the thread. *)

external get_state : t -> state =
  "ocamljava_thread_get_state"
(** Returns the state of the thread. *)

external get_thread_group : t -> ThreadGroup.t option =
  "ocamljava_thread_get_thread_group"
(** Returns the group of the thread, [None] if the thread has terminated
    its execution. *)

external interrupt : t -> unit =
  "ocamljava_thread_interrupt"
(** Interrupts the thread. *)

external interrupted : unit -> bool =
  "ocamljava_thread_interrupted"
(** Tests whether the current thread has been interrupted, and sets its
    interrupted status to [false]. *)

external is_alive : t -> bool =
  "ocamljava_thread_is_alive"
(** Tests whether the thread has started and not terminated. *)

external is_daemon : t -> bool =
  "ocamljava_thread_is_daemon"
(** Tests whether the thread is a daemon one. *)

external is_interrupted : t -> bool =
  "ocamljava_thread_is_interrupted"
(** Tests whether the thread has been interrupted. *)

external join : t -> unit =
  "ocamljava_thread_join"
(** Waits for the termination of the passed thread.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external join_time : t -> int64 -> unit =
  "ocamljava_thread_join_time"
(** [join_time t m] is similar to [join t], except that the current
    thread will at most wait for [m] milliseconds.

    Raises [Invalid_argument] if [m] is invalid.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external join_time_nanos : t -> int64 -> int32 -> unit =
  "ocamljava_thread_join_time_nanos"
(** [join_time t m n] is similar to [join t], except that the current
    thread will at most wait for [m] milliseconds plus [n] nanoseconds.

    Raises [Invalid_argument] if [m] or [n] is invalid.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external set_daemon : t -> bool -> unit =
  "ocamljava_thread_set_daemon"
(** Changes the daemon status of the thread.

    Raises [Invalid_argument] if the thread has already been started. *)

external set_name : t -> string -> unit =
  "ocamljava_thread_set_name"
(** Changes the name of the thread. *)

external set_priority : t -> int32 -> unit =
  "ocamljava_thread_set_priority"
(** Changes the priority of the thread.

    Raises [Invalid_argument] if the priority is invalid. *)

external sleep : int64 -> unit =
  "ocamljava_thread_sleep"
(** [sleep m] waits for [m] milliseconds.

    Raises [Invalid_argument] if [m] is invalid.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external sleep_nanos : int64 -> int32 -> unit =
  "ocamljava_thread_sleep_nanos"
(** [sleep m n] waits for [m] milliseconds plus [n] nanoseconds.

    Raises [Invalid_argument] if [m] or [n] is invalid.

    Raises [Runtime.Interrupted] if the thread is interrupted. *)

external start : t -> unit =
  "ocamljava_thread_start"
(** Starts the execution of the thread.

    Raises [Invalid_argument] if the thread has already been started. *)

external yield : unit -> unit =
  "ocamljava_thread_yield"
(** Indicates that the current thread wants to yield its use of a
    processor. *)
