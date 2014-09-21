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

(** Parallel operations over arrays.

    Most operations have the same signature as their original
    counterparts, but {i fold} operations need an additional function
    in order to be able to "merge" the computations done on sub arrays.

    Operations doing computations in parallel can be passed two optional
    parameters to set thread pool, and chunk size. It is possible to get
    rid of these optional parameters through the use of the [Make]
    functor.

    If any parallel operation is used with a given pool, it is necessary
    to shutdown the pool. This applies in particular to [default_pool],
    that can be shutdown through the [shutdown_now] function. *)


(** {6 Thread pool} *)

val default_pool : ThreadPoolExecutor.t
(** The default pool used for parallel computations, initialized with a
    size of [Runtime.available_processors]. *)

val shutdown_now : unit -> unit
(** Shutdowns [default_pool]. *)


(** {6 Synonyms for sequential operations} *)

external length : 'a array -> int =
  "%array_length"
(** Synonym for [Array.length]. *)

external get : 'a array -> int -> 'a =
  "%array_safe_get"
(** Synonym for [Array.get]. *)

external set : 'a array -> int -> 'a -> unit =
  "%array_safe_set"
(** Synonym for [Array.set]. *)

external make : int -> 'a -> 'a array =
  "caml_make_vect"
(** Synonym for [Array.make]. *)

external create : int -> 'a -> 'a array =
  "caml_make_vect"
(** Synonym for [Array.create]. *)

val make_matrix : int -> int -> 'a -> 'a array array
(** Synonym for [Array.make_matrix]. *)

val create_matrix : int -> int -> 'a -> 'a array array
(** Synonym for [Array.create_matrix]. *)

val append : 'a array -> 'a array -> 'a array
(** Synonym for [Array.append]. *)

val concat : 'a array list -> 'a array
(** Synonym for [Array.concat]. *)

val sub : 'a array -> int -> int -> 'a array
(** Synonym for [Array.sub]. *)

val copy : 'a array -> 'a array
(** Synonym for [Array.copy]. *)

val fill : 'a array -> int -> int -> 'a -> unit
(** Synonym for [Array.fill]. *)

val blit : 'a array -> int -> 'a array -> int -> int -> unit
(** Synonym for [Array.blit]. *)

val to_list : 'a array -> 'a list
(** Synonym for [Array.to_list]. *)

val of_list : 'a list -> 'a array
(** Synonym for [Array.of_list]. *)


(** {6 Parallel operations} *)

val init : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> int -> (int -> 'a) -> 'a array
(** Similar to [Array.init], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    Raise [Invalid_argument] if passed size is negative or above
    [Sys.max_array_length].

    Raises [Runtime.Raised] if the passed function raises an exception. *)

val iter : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> unit) -> 'a array -> unit
(** Similar to [Array.iter], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    Raises [Runtime.Raised] if the passed function raises an exception. *)

val map : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> 'b) -> 'a array -> 'b array
(** Similar to [Array.map], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    Raises [Runtime.Raised] if the passed function raises an exception. *)

val iteri : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> (int -> 'a -> unit) -> 'a array -> unit
(** Similar to [Array.iteri], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    Raises [Runtime.Raised] if the passed function raises an exception. *)

val mapi : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> (int -> 'a -> 'b) -> 'a array -> 'b array
(** Similar to [Array.mapi], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    Raises [Runtime.Raised] if the passed function raises an exception. *)

val fold_left : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> 'b -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'b array -> 'a
(** Similar to [Array.fold_left], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    {i This version uses an additional function in order to be able to
    "merge" the computations done on sub arrrays.}

    Raises [Runtime.Raised] if any of the passed functions raises an exception. *)

val fold_right : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('b -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'b array -> 'a -> 'a
(** Similar to [Array.fold_right], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    {i This version uses an additional function in order to be able to
    "merge" the computations done on sub arrrays.}

    Raises [Runtime.Raised] if any of the passed functions raises an exception. *)

val sort : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> 'a -> int) -> 'a array -> unit
(** Similar to [Array.sort], except that computations are done in
    parallel; [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    The current implementation has a {i O(n)} space complexity.

    Raises [Runtime.Raised] if the passed function raises an exception. *)

val stable_sort : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> 'a -> int) -> 'a array -> unit
(** Synonym for {!ParallelArray.sort}. *)

val fast_sort : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> 'a -> int) -> 'a array -> unit
(** Synonym for {!ParallelArray.sort}. *)


(** {6 Additional parallel operations} *)

val mem : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> 'a -> 'a array -> bool
(** [mem ~pool ~chunk_size x a] returns [true] iff there is an element
    [e] of [a] such that [e = x]. [pool] indicates the thread pool to use
    (defaulting to [default_pool]), while [chunk_size] indicates the size
    of each chunk to be allocated to a thread. *)

val memq : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> 'a -> 'a array -> bool
(** [memq ~pool ~chunk_size x a] returns [true] iff there is an element
    [e] of [a] such that [e == x]. [pool] indicates the thread pool to use
    (defaulting to [default_pool]), while [chunk_size] indicates the size
    of each chunk to be allocated to a thread. *)

val exists : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> bool) -> 'a array -> bool
(** [exists ~pool ~chunk_size f a] returns [true] iff there is an element
    [e] of [a] such that [f e = true]. [pool] indicates the thread pool
    to use (defaulting to [default_pool]), while [chunk_size] indicates
    the size of each chunk to be allocated to a thread. *)

val for_all : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> bool) -> 'a array -> bool
(** [for_all ~pool ~chunk_size f a] returns [true] iff there is no element
    [e] of [a] such that [f e = false]. [pool] indicates the thread pool
    to use (defaulting to [default_pool]), while [chunk_size] indicates
    the size of each chunk to be allocated to a thread. *)

val find : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> bool) -> 'a array -> 'a
(** [find ~pool ~chunk_size f a] returns an element [e] of [a] such that
    [f e = true], raising [Not_found] if there is no such element in [a].
    [pool] indicates the thread pool to use (defaulting to
    [default_pool]), while [chunk_size] indicates the size of each chunk
    to be allocated to a thread.

    {i Returns any element matching the condition, not guaranteeing that
    the one with the lowest index is returned.} *)

val find_index : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> bool) -> 'a array -> int
(** Similar to [find], except that the index is returned. *)

val find_all : ?pool:ThreadPoolExecutor.t -> ?chunk_size:int -> ('a -> bool) -> 'a array -> 'a list
(** [find_all ~pool ~chunk_size f a] returns all the elements of [a] that
    return [true] when applied to [f], preserving the order of the
    elements in [a]. *)


(** {6 Functor} *)

module type OptionalParameters = sig
  val pool : ThreadPoolExecutor.t
      (** The pool to be used for parallel computations. *)
  val chunk_size : int
      (** The size of chunks for parallel computations. *)
end
(** Signature of {!ParallelArray.Make} parameter. *)

module type S = sig
  external length : 'a array -> int = "%array_length"
  external get : 'a array -> int -> 'a = "%array_safe_get"
  external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
  external make : int -> 'a -> 'a array = "caml_make_vect"
  external create : int -> 'a -> 'a array = "caml_make_vect"
  val init : int -> (int -> 'a) -> 'a array
  val make_matrix : int -> int -> 'a -> 'a array array
  val create_matrix : int -> int -> 'a -> 'a array array
  val append : 'a array -> 'a array -> 'a array
  val concat : 'a array list -> 'a array
  val sub : 'a array -> int -> int -> 'a array
  val copy : 'a array -> 'a array
  val fill : 'a array -> int -> int -> 'a -> unit
  val blit : 'a array -> int -> 'a array -> int -> int -> unit
  val to_list : 'a array -> 'a list
  val of_list : 'a list -> 'a array
  val iter : ('a -> unit) -> 'a array -> unit
  val map : ('a -> 'b) -> 'a array -> 'b array
  val iteri : (int -> 'a -> unit) -> 'a array -> unit
  val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
  val fold_left : ('a -> 'b -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'b array -> 'a
  val fold_right : ('b -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'b array -> 'a -> 'a
  val sort : ('a -> 'a -> int) -> 'a array -> unit
  val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
  val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
  val mem : 'a -> 'a array -> bool
  val memq : 'a -> 'a array -> bool
  val exists : ('a -> bool) -> 'a array -> bool
  val for_all : ('a -> bool) -> 'a array -> bool
  val find : ('a -> bool) -> 'a array -> 'a
  val find_index : ('a -> bool) -> 'a array -> int
  val find_all : ('a -> bool) -> 'a array -> 'a list
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
end
(** Similar to [module type of Array], except for {i fold} operations. *)

module Make (OP : OptionalParameters) : S
(** Functor building an implementation with no optional parameter,
    thus compatible with the [Array] module (except for {i fold}
    operations). *)

(**/**)

external unsafe_get : 'a array -> int -> 'a =
  "%array_unsafe_get"
(** Synonym for {!Array.unsafe_get}. *)

external unsafe_set : 'a array -> int -> 'a -> unit =
  "%array_unsafe_set"
(** Synonym for {!Array.unsafe_set}. *)
