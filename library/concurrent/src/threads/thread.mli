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
  | New           (** The thread has not been started. *)
  | Runnable      (** The thread is executing. *)
  | Blocked       (** The thread is blocked on a lock. *)
  | Waiting       (** The thread is waiting for another thread (with no timeout). *)
  | Timed_waiting (** The thread is waiting for another thread (with a timeout). *)
  | Terminated    (** The thread has terminated execution. *)
(** The type of thread states; see {java java.lang.Thread.State}. *)

val max_priority : java_int
(** The maximum priority for a thread; see
    {java java.lang.Thread#MAX_PRIORITY}. *)

val min_priority : java_int
(** The minimum priority for a thread; see
    {java java.lang.Thread#MIN_PRIORITY}. *)

val norm_priority : java_int
(** The normal ({i i. e. default}) priority for a thread; see
    {java java.lang.Thread#NORM_PRIORITY}. *)

type t = java'lang'Thread java_instance
(** The type of threads. If a thread raises an exception escapes, it is
    written to the standard output and then discarded. *)

val make : ?group:ThreadGroup.t -> ?name:JavaString.t -> ('a -> unit) -> 'a -> t
(** [make g n f x] returns a new thread in group [g] with name [n]. The
    thread will execute [f x] when started; see
    {java java.lang.Thread#Thread(java.lang.ThreadGroup, java.lang.Runnable, java.lang.String)}. *)

val current_thread : unit -> t
(** Returns the thread that is currently executing; see
    {java java.lang.Thread#currentThread()}. *)

val get_id : t -> java_long
(** Returns the thread identifier. The identifier is positive, and
    guaranteed to be unique while the thread is executing. The identifier
    can be recycled when the thread has terminated its execution; see
    {java java.lang.Thread#getId()}. *)

val get_name : t -> JavaString.t
(** Returns the name of the thread; see
    {java java.lang.Thread#getName()}. *)

val get_priority : t -> java_int
(** Returns the priority of the thread; see
    {java java.lang.Thread#getPriority()}. *)

val get_state : t -> state
(** Returns the state of the thread; see
    {java java.lang.Thread#getState()}. *)

val get_thread_group : t -> ThreadGroup.t
(** Returns the group of the thread, [None] if the thread has terminated
    its execution; see {java java.lang.Thread#getThreadGroup()}. *)

val interrupt : t -> unit
(** Interrupts the thread; see {java java.lang.Thread#interrupt()}. *)

val interrupted : unit -> bool
(** Tests whether the current thread has been interrupted, and sets its
    interrupted status to [false]; see
    {java java.lang.Thread#interrupted()}. *)

val is_alive : t -> bool
(** Tests whether the thread has started and not terminated; see
    {java java.lang.Thread#isAlive()}. *)

val is_daemon : t -> bool
(** Tests whether the thread is a daemon one; see
    {java java.lang.Thread#isDaemon()}. *)

val is_interrupted : t -> bool
(** Tests whether the thread has been interrupted; see
    {java java.lang.Thread#isInterrupted()}. *)

val join : t -> unit
(** Waits for the termination of the passed thread; see
    {java java.lang.Thread#join()}.

    @raise Java_exception if the thread is interrupted *)

val join_time : t -> java_long -> unit
(** [join_time t m] is similar to [join t], except that the current
    thread will at most wait for [m] milliseconds; see
    {java java.lang.Thread#join(long)}.

    @raise Java_exception if [m] is invalid
    @raise Java_exception if the thread is interrupted *)

val join_time_nanos : t -> java_long -> java_int -> unit
(** [join_time t m n] is similar to [join t], except that the current
    thread will at most wait for [m] milliseconds plus [n] nanoseconds; see
    {java java.lang.Thread#join(long, int)}.

    @raise Java_exception if [m] or [n] is invalid
    @raise Java_exception if the thread is interrupted *)

val set_daemon : t -> bool -> unit
(** Changes the daemon status of the thread; see
    {java java.lang.Thread#setDaemon(boolean)}.

    @raise Java_exception if the thread has already been started *)

val set_name : t -> JavaString.t -> unit
(** Changes the name of the thread; see
    {java java.lang.Thread#setName(java.lang.String)}. *)

val set_priority : t -> java_int -> unit
(** Changes the priority of the thread; see
    {java java.lang.Thread#setPriority(int)}.

    @raise Java_exception if the priority is invalid *)

val sleep : java_long -> unit
(** [sleep m] waits for [m] milliseconds; see
    {java java.lang.Thread#sleep(long)}.

    @raise Java_exception if [m] is invalid
    @raise Java_exception if the thread is interrupted *)

val sleep_nanos : java_long -> java_int -> unit
(** [sleep m n] waits for [m] milliseconds plus [n] nanoseconds; see
    {java java.lang.Thread#sleep(long, int)}.

    @raise Java_exception if [m] or [n] is invalid
    @raise Java_exception if the thread is interrupted *)

val start : t -> unit
(** Starts the execution of the thread; see
    {java java.lang.Thread#start()}.

    @raise Java_exception if the thread has already been started *)

val yield : unit -> unit
(** Indicates that the current thread wants to yield its use of a
    processor; see {java java.lang.Thread#yield()}. *)


(** {6 Null value} *)

val null : t
(** The [null] value. *)

external is_null : t -> bool =
  "java is_null"
(** [is_null obj] returns [true] iff [obj] is equal to [null]. *)

external is_not_null : t -> bool =
  "java is_not_null"
(** [is_not_null obj] returns [false] iff [obj] is equal to [null]. *)


(** {6 Miscellaneous} *)

val wrap : t -> t option
(** [wrap obj] wraps the reference [obj] into an option type:
    - [Some x] if [obj] is not [null];
    - [None] if [obj] is [null]. *)

val unwrap : t option -> t
(** [unwrap obj] unwraps the option [obj] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
