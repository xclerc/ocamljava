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

(* Thread pool *)

let parallelism = Runtime.available_processors ()

let parallelism_int = Int32.to_int parallelism

let default_pool =
  ThreadPoolExecutor.make
    parallelism (Int32.mul 2l parallelism)
    1L TimeUnit.Seconds
    RejectedExecutionHandler.Caller_runs_policy

let shutdown_now () =
  ignore (ThreadPoolExecutor.shutdown_now default_pool)


(* Synonyms for sequential operations *)

external unsafe_get : 'a array -> int -> 'a =
  "%array_unsafe_get"

external unsafe_set : 'a array -> int -> 'a -> unit =
  "%array_unsafe_set"

external length : 'a array -> int =
  "%array_length"

external get : 'a array -> int -> 'a =
  "%array_safe_get"

external set : 'a array -> int -> 'a -> unit =
  "%array_safe_set"

external make : int -> 'a -> 'a array =
  "caml_make_vect"

external create : int -> 'a -> 'a array =
  "caml_make_vect"

let make_matrix x y z = Array.make_matrix x y z

let create_matrix x y z = Array.create_matrix x y z

let append x y = Array.append x y

let concat x = Array.concat x

let sub x y z = Array.sub x y z

let copy x = Array.copy x

let fill x y z t = Array.fill x y z t

let blit x y z t u = Array.blit x y z t u

let to_list x = Array.to_list x

let of_list x = Array.of_list x


(* Parallel operations *)

let heuristic_size len = max 16 (len / (parallelism_int * 2))

let rec wait_completion = function
  | hd :: tl -> Future.get hd; wait_completion tl
  | [] -> ()

let wait_completion_exn l =
  try
    wait_completion l
  with Runtime.Raised e ->
    raise e

let init_part f a low high () =
  for i = low to pred high do
    unsafe_set a i (f i)
  done

let init ?(pool = default_pool) ?(chunk_size = -1) len f =
  if len = 0 then
    [||]
  else begin
    let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
    let f0 = f 0 in
    let a = Array.create len f0 in
    let futures = ref [] in
    let low = ref 1 in
    while (!low < len) do
      let high = min (!low + sz) len in
      let future =
        ThreadPoolExecutor.submit
          pool
          (init_part f a !low high)
        () in
      futures := future :: !futures;
      low := !low + sz
    done;
    wait_completion_exn !futures;
    a
  end

let iter_part f a low high () =
  for i = low to pred high do
    f (unsafe_get a i)
  done

let iter ?(pool = default_pool) ?(chunk_size = -1) f a =
  let len = length a in
  let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
  let low = ref 0 in
  let futures = ref [] in
  while (!low < len) do
    let high = min (!low + sz) len in
    let future = ThreadPoolExecutor.submit
        pool
        (iter_part f a !low high)
        () in
    futures := future :: !futures;
    low := !low + sz
  done;
  wait_completion_exn !futures

let map_part f a r low high () =
  for i = low to pred high do
    unsafe_set r i (f (unsafe_get a i))
  done

let map ?(pool = default_pool) ?(chunk_size = -1) f a =
  let len = length a in
  if len = 0 then
    [||]
  else begin
    let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
    let f0 = f (unsafe_get a 0) in
    let r = Array.create len f0 in
    let futures = ref [] in
    let low = ref 1 in
    while (!low < len) do
      let high = min (!low + sz) len in
      let future =
        ThreadPoolExecutor.submit
          pool
          (map_part f a r !low high)
        () in
      futures := future :: !futures;
      low := !low + sz
    done;
    wait_completion_exn !futures;
    r
  end

let iteri_part f a low high () =
  for i = low to pred high do
    f i (unsafe_get a i)
  done

let iteri ?(pool = default_pool) ?(chunk_size = -1) f a =
  let len = length a in
  let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
  let low = ref 0 in
  let futures = ref [] in
  while (!low < len) do
    let high = min (!low + sz) len in
    let future = ThreadPoolExecutor.submit
        pool
        (iteri_part f a !low high)
        () in
    futures := future :: !futures;
    low := !low + sz
  done;
  wait_completion_exn !futures

let mapi_part f a r low high () =
  for i = low to pred high do
    unsafe_set r i (f i (unsafe_get a i))
  done

let mapi ?(pool = default_pool) ?(chunk_size = -1) f a =
  let len = length a in
  if len = 0 then
    [||]
  else begin
    let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
    let f0 = f 0 (unsafe_get a 0) in
    let r = Array.create len f0 in
    let futures = ref [] in
    let low = ref 1 in
    while (!low < len) do
      let high = min (!low + sz) len in
      let future =
        ThreadPoolExecutor.submit
          pool
          (mapi_part f a r !low high)
        () in
      futures := future :: !futures;
      low := !low + sz
    done;
    wait_completion_exn !futures;
    r
  end

let fold_left_part f x a low high () =
  let res = ref x in
  for i = low to pred high do
    res := f !res (unsafe_get a i)
  done;
  !res

let fold_left ?(pool = default_pool) ?(chunk_size = -1) f g x a =
  let len = length a in
  let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
  let low = ref 0 in
  let futures = ref [] in
  while (!low < len) do
    let high = min (!low + sz) len in
    let future = ThreadPoolExecutor.submit
        pool
        (fold_left_part f x a !low high)
        () in
    futures := future :: !futures;
    low := !low + sz
  done;
  let futures = ref (List.rev !futures) in
  let acc = ref x in
  while !futures <> [] do
    (try
      acc := g !acc (Future.get (List.hd !futures))
    with Runtime.Raised e -> raise e);
    futures := List.tl !futures
  done;
  !acc

let fold_right_part f a x low high () =
  let res = ref x in
  for i = pred high downto low do
    res := f (unsafe_get a i) !res
  done;
  !res

let fold_right ?(pool = default_pool) ?(chunk_size = -1) f g a x =
  let len = length a in
  let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
  let low = ref 0 in
  let futures = ref [] in
  while (!low < len) do
    let high = min (!low + sz) len in
    let future = ThreadPoolExecutor.submit
        pool
        (fold_right_part f a x !low high)
        () in
    futures := future :: !futures;
    low := !low + sz
  done;
  let acc = ref x in
  while !futures <> [] do
    (try
      acc := g (Future.get (List.hd !futures)) !acc
    with Runtime.Raised e -> raise e);
    futures := List.tl !futures
  done;
  !acc

let sort ?(pool = default_pool) ?(chunk_size = -1) cmp a =
  let len = length a in
  if len > 1 then begin
    let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
    let low = ref 0 in
    let futures = ref [] in
    let parts = ref [] in
    while (!low < len) do
      let high = min (!low + sz) len in
      let l = high - !low in
      let tmp = Array.sub a !low l in
      let future = ThreadPoolExecutor.submit
          pool
          (Array.sort cmp)
          tmp in
      futures := future :: !futures;
      parts := tmp :: !parts;
      low := !low + sz
    done;
    let parts = Array.of_list !parts in
    let p = Array.length parts in
    let indices = Array.make p 0 in
    let lengths = Array.make p 0 in
    for i = 0 to pred p do
      lengths.(i) <- Array.length parts.(i)
    done;
    (try
      List.iter Future.get !futures
    with Runtime.Raised e -> raise e);
    for i = 0 to pred len do
      let j = ref 0 in
      while indices.(!j) = lengths.(!j) do
        incr j
      done;
      let min = ref parts.(!j).(indices.(!j)) in
      let idx = ref !j in
      for k = succ !j to pred p do
        if indices.(k) < lengths.(k) then begin
          let elem = parts.(k).(indices.(k)) in
          if elem < !min then begin
            min := elem;
            idx := k
          end
        end
      done;
      a.(i) <- !min;
      indices.(!idx) <- indices.(!idx) + 1
    done
  end

let stable_sort ?(pool = default_pool) ?(chunk_size = -1) cmp a =
  sort ~pool ~chunk_size cmp a

let fast_sort ?(pool = default_pool) ?(chunk_size = -1) cmp a =
  sort ~pool ~chunk_size cmp a


(* Additional parallel operations *)

let rec wait_successful_completion = function
  | hd :: tl ->
      (match Future.get hd with
      | (Some _) as res ->
          List.iter (fun f -> ignore (Future.cancel f true)) tl;
          res
      | None ->
          wait_successful_completion tl)
  | [] -> None

let wait_successful_completion_exn l =
  try
    wait_successful_completion l
  with Runtime.Raised e ->
    raise e

let search_part predicate a low high () =
  let index = ref low in
  let interrupted = ref (Thread.interrupted ()) in
  while (!index < high)
      && (not !interrupted)
      && (not (predicate (unsafe_get a !index))) do
    incr index;
    interrupted := Thread.interrupted ()
  done;
  if (!index < high) && (not !interrupted) then
    Some !index
  else
    None

let search ?(pool = default_pool) ?(chunk_size = -1) predicate a =
  let len = length a in
  let sz = if chunk_size < 1 then heuristic_size len else chunk_size in
  let low = ref 0 in
  let futures = ref [] in
  while (!low < len) do
    let high = min (!low + sz) len in
    let future = ThreadPoolExecutor.submit
        pool
        (search_part predicate a !low high)
        () in
    futures := future :: !futures;
    low := !low + sz
  done;
  wait_successful_completion_exn !futures

let mem ?(pool = default_pool) ?(chunk_size = -1) x a =
  let res = search ~pool ~chunk_size (fun e -> e = x) a in
  match res with
  | Some _ -> true
  | None -> false

let memq ?(pool = default_pool) ?(chunk_size = -1) x a =
  match search ~pool ~chunk_size (fun e -> e == x) a with
  | Some _ -> true
  | None -> false

let exists ?(pool = default_pool) ?(chunk_size = -1) f a =
  match search ~pool ~chunk_size f a with
  | Some _ -> true
  | None -> false

let for_all ?(pool = default_pool) ?(chunk_size = -1) f a =
  match search ~pool ~chunk_size (fun e -> not (f e)) a with
  | Some _ -> false
  | None -> true

let find ?(pool = default_pool) ?(chunk_size = -1) f a =
  match search ~pool ~chunk_size f a with
  | Some x -> unsafe_get a x
  | None -> raise Not_found

let find_index ?(pool = default_pool) ?(chunk_size = -1) f a =
  match search ~pool ~chunk_size f a with
  | Some x -> x
  | None -> raise Not_found

let find_all ?(pool = default_pool) ?(chunk_size = -1) f a =
  fold_right ~pool ~chunk_size
    (fun elem acc -> if f elem then elem :: acc else acc)
    (@)
    a
    []


(* Functor *)

module type OptionalParameters = sig
  val pool : ThreadPoolExecutor.t
  val chunk_size : int
end

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

module Make (OP : OptionalParameters) = struct
  external length : 'a array -> int = "%array_length"
  external get : 'a array -> int -> 'a = "%array_safe_get"
  external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
  external make : int -> 'a -> 'a array = "caml_make_vect"
  external create : int -> 'a -> 'a array = "caml_make_vect"
  let init = Array.init
  let make_matrix = Array.make_matrix
  let create_matrix = Array.create_matrix
  let append = Array.append
  let concat = Array.concat
  let sub = Array.sub
  let copy = Array.copy
  let fill = Array.fill
  let blit = Array.blit
  let to_list = Array.to_list
  let of_list = Array.of_list
  let iter x y = iter ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let map x y = map ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let iteri x y = iteri ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let mapi x y = mapi ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let fold_left x y z t = fold_left ~pool:OP.pool ~chunk_size:OP.chunk_size x y z t
  let fold_right x y z t = fold_right ~pool:OP.pool ~chunk_size:OP.chunk_size x y z t
  let sort x y = sort ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let stable_sort x y = stable_sort ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let fast_sort x y = fast_sort ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let mem x y = mem ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let memq x y = memq ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let exists x y = exists ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let for_all x y = for_all ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let find x y = find ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let find_index x y = find_index ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  let find_all x y = find_all ~pool:OP.pool ~chunk_size:OP.chunk_size x y
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
end
