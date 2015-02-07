(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2015 Xavier Clerc.
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


let error kind msg =
  Printf.eprintf "*** %s: %s\n" kind msg;
  exit 1

let main () =
  Args.parse ();
  match !Args.files with
  | [ file_out; file_in ] ->
      State.input_file := file_in;
      let file_in = Path.make_of_string file_in in
      let file_out = Path.make_of_string file_out in
      WholeProgram.iter_archive file_in;
      let make_mapper arch =
        WholeProgram.make_remove_indices_function ()
        |> new Mapper.for_archive arch in
      let post arch =
        let path, contents = WholeProgram.compile_shared_constant_class () in
        ArchiveOutputStream.add_entry arch path contents in
      ArchiveTraversal.map_file make_mapper ~post file_in file_out
  | _ ->
      error
        "error"
        "input and output files should be provided on the command line"

let () =
  try
    main ();
    exit 0
  with
  | StackState.Exception e ->
      error "stack state exception" (StackState.string_of_error e)
  | Annotation.Exception e ->
      error "annotation exception" (Annotation.string_of_error e)
  | Sys_error se ->
      error "system error" se
  | e ->
      error "error" (Printexc.to_string e)
