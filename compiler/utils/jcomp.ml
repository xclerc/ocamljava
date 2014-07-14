(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java compiler is free software; you can redistribute it and/or modify
 * it under the terms of the Q Public License as published by
 * Trolltech (with a change to choice of law).
 *
 * OCaml-Java compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Q Public License for more details.
 *
 * You should have received a copy of the Q Public License
 * along with this program.  If not, see
 * <http://opensource.org/licenses/QPL-1.0>.
 *)


open BaristaLibrary

let compile_file name =
  (* There is no needed to check 'Jclflags.classpath_reset' because it is
     only used to know whether to include the elements of the system
     classpath, and we never have to pass such elements to the Java
     compiler. *)
  let separator =
    ClassPath.default_separator
    |> UChar.to_char
    |> String.make 1 in
  let classpath =
    ((Jconfig.get_runtime_jar ()) :: (List.rev !Jclflags.classpath))
    |> String.concat separator in
  let options =
    match !Clflags.all_ccopts with
    | _ :: _ ->
        let opts =
          !Clflags.all_ccopts
          |> List.rev
          |> String.concat " " in
        " " ^ opts
    | [] ->
        "" in
  let command =
    Printf.sprintf "%s -cp %s%s %s"
      (match !Clflags.c_compiler with
      | Some x -> x
      | None -> "javac")
      classpath
      options
      (Filename.quote name) in
  Ccomp.command command
