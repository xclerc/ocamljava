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

type transaction

type 'a ref

external init : unit -> unit =
  "ocamljava_stm_init"

external make_transaction : unit -> transaction =
  "ocamljava_stm_make_transaction"

external make_ref : 'a -> 'a ref =
  "ocamljava_stm_make_ref"

external get : transaction -> 'a ref -> 'a =
  "ocamljava_stm_get"

external set : transaction -> 'a ref -> 'a -> unit =
  "ocamljava_stm_set"

external commit : transaction -> unit =
  "ocamljava_stm_commit"

external invalidate : transaction -> unit =
  "ocamljava_stm_invalidate"

exception Retry

let () = Callback.register_exception "Concurrent.STM.Retry" Retry

exception Abort

exception Cancelled

let ref x = make_ref x

let default_retries = 64

let rec run ?(retries = default_retries) f =
  let trans = make_transaction () in
  let getter = get trans in
  let setter = set trans in
  try
    let res = f getter setter in
    commit trans;
    res
  with
  | Retry ->
      if retries > 0 then
        run ~retries:(pred retries) f
      else
        raise Cancelled
  | e ->
      invalidate trans;
      raise e

let rec run_read_only ?(retries = default_retries) f =
  let trans = make_transaction () in
  let getter = get trans in
  try
    f getter
  with
  | Retry ->
      if retries > 0 then
        run_read_only ~retries:(pred retries) f
      else
        raise Cancelled
  | e ->
      invalidate trans;
      raise e

let () = init ()
