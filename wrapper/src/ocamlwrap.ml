(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


let main () =
  Args.parse ();
  Config.load_path :=
    [ "."; Config.standard_library ]
    @ (List.rev !Args.includes)
    @ !Config.load_path;
  List.iter
    (fun { Args.file_name; file_package } ->
      let packname=
        match file_package with
        | Some _ -> file_package
        | None   -> !Args.package in
      Wrap.file file_name packname)
    (List.rev !Args.files)

let error kind msg =
  Printf.eprintf "*** %s: %s\n" kind msg;
  exit 1

let () =
  try
    main ();
    exit 0
  with
  | Wrap_common.Exception e ->
      error "error" (Wrap_common.string_of_error e)
  | Sys_error se ->
      error "system error" se
  | e ->
      error "error" (Printexc.to_string e)
