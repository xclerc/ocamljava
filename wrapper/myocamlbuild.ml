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

open Ocamlbuild_plugin

let () =
  dispatch begin function
    | After_rules ->
        flag ["ocaml"; "compile"; "warnings"]
          (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        flag ["ocaml"; "compile"; "use_compiler_libs"]
          (S[A"-I"; A"+compiler-libs"]);
        flag ["ocaml"; "link"; "byte"; "use_compiler_libs"]
          (S[A"unix.cma";
             A"-I"; A"+zip"; A"zip.cma";
             A"bigarray.cma";
             A"camomile.cma";
             A"-I"; A"+barista"; A"baristaLibrary.cma";
             A"-I"; A"+compiler-libs"; A"ocamlcommon.cma"; A"ocamljavacomp.cma"]);
        flag ["ocaml"; "link"; "native"; "use_compiler_libs"]
          (S[A"unix.cmxa";
             A"-I"; A"+zip"; A"zip.cmxa";
             A"bigarray.cmxa";
             A"camomile.cmxa";
             A"-I"; A"+barista"; A"baristaLibrary.cmxa";
             A"-I"; A"+compiler-libs"; A"ocamlcommon.cmxa"; A"ocamljavacomp.cmxa"]);
        flag ["ocaml"; "link"; "java"; "use_compiler_libs"]
          (S[A"-I"; A"+barista"; A"baristaLibrary.cmja";
             A"-I"; A"+compiler-libs"; A"ocamlcommon.cmja"; A"ocamljavacomp.cmja"]);
    | _ -> ()
  end
