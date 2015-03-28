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

(** Fork/join computations. *)

external split : ForkJoinPool.t -> ('a -> ('a * 'a) option) -> ('b -> 'b -> 'b) -> ('a -> 'b) -> 'a -> 'b =
  "ocamljava_forkjoin_split"
(** [split pool fork join f x] computes [f x] by leveraging multiple
    threads from [pool].

    The [fork] function is used to determine for a given input value
    whether the computation should be split (returning [Some (x1, x2)]
    will generate two sub-computations with inputs [x1] and [x2]) or not
    (returning [None]). The [fork] function is recursively called inside
    sub-computations.

    The [join] function is used to combine the results of two
    sub-computations.

    Raises an exception if any call of [fork], [join], or [f] raises an
    uncaught exception, or if the passed pool cannot execute the
    computation.

    As an example, a (very inefficient) way to compute the fibonacci
    function using several threads is:

    {[
    let rec fib n =
      if n <= 1 then
        1
      else
        (fib (n - 2)) + (fib (n - 1))

    let threshold = 10

    let fork n = if n < threshold then None else Some (n - 1, n - 2)

    let join x y = x + y

    let parallel_fib pool = split pool fork join fib
    ]}. *)

external split_list : ForkJoinPool.t -> ('a -> 'a list) -> ('b -> 'b -> 'b) -> ('a -> 'b) -> 'a -> 'b =
  "ocamljava_forkjoin_split_list"
(** [split_list pool fork join f x] is similar to [split pool fork join f x],
    except that the [fork] function can generate more than two
    sub-computations. *)

external split_array : ForkJoinPool.t -> ('a -> 'a array) -> ('b -> 'b -> 'b) -> ('a -> 'b) -> 'a -> 'b =
  "ocamljava_forkjoin_split_array"
(** [split_array pool fork join f x] is similar to [split pool fork join f x],
    except that the [fork] function can generate more than two
    sub-computations. *)
