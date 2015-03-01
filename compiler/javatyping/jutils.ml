(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
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


let ocamldoc_mode = ref false

let class_loader = ref None

let get_class_loader () =
  match !class_loader with
  | Some x -> x
  | None ->
      let initial_classpath =
        if !Jclflags.classpath_reset then
          ClassPath.make_empty ()
        else
          ClassPath.make () in
      let class_path_elements = (Jconfig.get_runtime_jar ()) :: (List.rev !Jclflags.classpath) in
      let class_path_elements =
        if !Jclflags.servlet <> None then
          let servlet_basename =
            Printf.sprintf "javax.servlet-api-%s.jar" Jconfig.servlet_version in
          let servlet_jar =
            Filename.concat
              Config.standard_library
              (Filename.concat "external-jars" servlet_basename) in
          servlet_jar :: class_path_elements
        else
          class_path_elements in
      let classpath =
        List.fold_left
          (fun acc elem -> ClassPath.append (UTF8.of_string elem) acc)
          initial_classpath
          class_path_elements in
      let loader = ClassLoader.make_of_class_path classpath in
      class_loader := Some loader;
      loader

let unifier = ref None

let get_unifier () =
  match !unifier with
  | Some x -> x
  | None ->
      let res =
        StackState.unify_to_closest_common_parent
          (get_class_loader ())
          [] in
      unifier := Some res;
      res

let is_subtype x y = (* true iff x is a subtype of y *)
  if Name.equal_for_class x y then
    true
  else
    let loader = get_class_loader () in
    x
    |> ClassLoader.find_class_name loader
    |> Hierarchy.all_parent_class_names true loader
    |> List.exists (fun cn -> Name.equal_for_class cn y)

let use_dots s =
  let len = String.length s in
  let res = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    match s.[!i] with
    | '\'' when (!i + 1 < len) && (s.[!i + 1] = '\'') ->
        Buffer.add_char res '$';
        i := !i + 2
    | '\'' ->
        Buffer.add_char res '.';
        incr i
    | ch ->
        Buffer.add_char res ch;
        incr i
  done;
  Buffer.contents res

let use_single_quotes s =
  let len = String.length s in
  let res = Buffer.create len in
  for i = 0 to pred len do
    match s.[i] with
    | '.' -> Buffer.add_char res '\''
    | '$' -> Buffer.add_char res '\''; Buffer.add_char res '\''
    | ch  -> Buffer.add_char res ch
  done;
  Buffer.contents res
