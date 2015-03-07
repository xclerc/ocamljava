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

open Ocamlbuild_plugin

let odocl_file = Pathname.pwd / "concurrent.odocl"
let mllib_file = Pathname.pwd / "concurrent.mllib"
let src_path   = Pathname.pwd / "src"
let excluded_modules = []

let () =
  let odocl_chan = open_out odocl_file in
  let mllib_chan = open_out mllib_file in
  let add_file filename =
    if (Pathname.check_extension filename "mli")
      || (Pathname.check_extension filename "mly")
      || (Pathname.check_extension filename "mll") then begin
          let modulename = Pathname.remove_extension filename in
          let modulename = Pathname.basename modulename in
          let modulename = String.capitalize modulename in
          if not (List.mem modulename excluded_modules) then begin
            output_string odocl_chan modulename;
            output_char odocl_chan '\n';
            output_string mllib_chan modulename;
            output_char mllib_chan '\n'
          end
      end in
  Array.iter
    (fun path ->
      let path = src_path / path in
      if Pathname.is_directory path then
        Array.iter add_file (Pathname.readdir path)
      else
        add_file path)
    (Pathname.readdir src_path);
  close_out_noerr odocl_chan;
  close_out_noerr mllib_chan

let () =
  dispatch begin function
    | After_rules ->
        flag ["ocaml"; "compile"; "warnings"] (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        dep ["needs-runtime"] ["src/runtime.cmj"];
    | _ -> ()
  end
