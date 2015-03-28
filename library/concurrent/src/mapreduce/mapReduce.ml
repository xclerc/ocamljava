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

module type Computation = sig
  type input
  type key
  type value
  type output
  val compare_keys : key -> key -> int
  val map : input -> (key * value) list
  val combine : key -> value -> value -> value
  val reduce : key -> value -> output -> output
end

module type S = sig
  type input
  type output
  val compute : ThreadPoolExecutor.t -> input Stream.t -> output -> output
end

module Make (C : Computation) = struct

  type input = C.input

  type output = C.output

  module KeyMap = Map.Make (struct type t = C.key let compare = C.compare_keys end)

  let compute pool inputs =
    let n = max 1 (Int32.to_int (ThreadPoolExecutor.get_maximum_pool_size pool)) in
    let service = ExecutorCompletionService.make pool in
    let init = Stream.npeek n inputs in
    let futures =
      List.map
        (fun x -> ExecutorCompletionService.submit service C.map x)
        init in
    let running = ref (List.length futures) in
    for _i = 1 to !running do
      Stream.junk inputs;
    done;
    let map = ref KeyMap.empty in
    while !running > 0 do
      let finished = ExecutorCompletionService.take service in
      let kv_list : (C.key * C.value) list = Future.get finished in
      List.iter
        (fun (k, v) ->
          try
            let old = KeyMap.find k !map in
            map := KeyMap.add k (C.combine k old v) !map
          with Not_found ->
            map := KeyMap.add k v !map)
        kv_list;
      match Stream.peek inputs with
      | Some i ->
          Stream.junk inputs;
          ignore (ExecutorCompletionService.submit service C.map i)
      | None -> decr running
    done;
    KeyMap.fold C.reduce !map

end
