(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java optimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java optimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open BaristaLibrary


let input_file = ref "UNINITALIZED"

let current_class_name = ref (Misc.make_class_name_ext "UNINITALIZED")

let class_loader = ref None

let get_class_loader () =
  match !class_loader with
  | Some x -> x
  | None ->
      let cl =
        ClassPath.make ()
        |> ClassPath.append (UTF8.of_string !input_file)
        |> ClassLoader.make_of_class_path in
      class_loader := Some cl;
      cl

let unifier = ref None

let get_unifier () =
  match !unifier with
  | Some x -> x
  | None ->
      let unif =
        StackState.unify_to_closest_common_parent
          (get_class_loader ())
          [] in
      unifier := Some unif;
      unif
