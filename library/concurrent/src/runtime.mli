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

(** Access to miscellaneous runtime values. *)


(** {6 Time} *)

external current_time_millis : unit -> int64 =
  "ocamljava_runtime_current_time_millis"
(** Returns the current time in milliseconds. *)

external nano_time : unit -> int64 =
  "ocamljava_runtime_nano_time"
(** Similar to [current_time_millis], with greater resolution. *)


(** {6 Processors} *)

external available_processors : unit -> int32 =
  "ocamljava_runtime_available_processors"
(** Returns the number of (logical) available processors. *)


(** {6 Memory} *)

external free_memory : unit -> int64 =
  "ocamljava_runtime_free_memory"
(** Returns the amount of free memory in the JVM (in bytes). *)

external max_memory : unit -> int64 =
  "ocamljava_runtime_max_memory"
(** Returns the maximum amount of memory to be used by the JVM (in bytes). *)

external total_memory : unit -> int64 =
  "ocamljava_runtime_total_memory"
(** Returns the total amount of memory used by the JVM (in bytes). *)


(** {6 Exceptions} *)

exception Interrupted of string
(** Raised when a function is interrupted, using the string parameter to
    indicate the name of the function. *)

exception Timeout of string
(** Raised when a function has been waiting for the allocated amount of
    time, using the string parameter to indicate the name of the
    function. *)

exception Raised of exn
(** Raised when a passed function raised an exception, using the
    parameter to store the originally raised exception. *)
