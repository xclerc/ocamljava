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

let open_builder archive =
  archive
  |> Path.make_of_string
  |> ArchiveBuilder.make ~fail_on_duplicate:true

let add_entry builder name bytes =
  ArchiveBuilder.add_entry builder (UTF8.of_string name) bytes

let add_entry_from_file builder name file =
  ArchiveBuilder.add_entry_from_file builder (UTF8.of_string name) (Path.make_of_string file)

let close builder =
  ArchiveBuilder.close builder

let close_noerr builder =
  ArchiveBuilder.close_noerr builder

let copy_entries_list archive_list archive =
  let builder =
    try
      Some (open_builder archive)
    with _ ->
      None in
  match builder with
  | Some builder ->
      (try
        archive_list
        |> List.map Path.make_of_string
        |> ArchiveBuilder.add_entries_from_archive_files builder;
        ArchiveBuilder.close builder;
        None
      with
      | ArchiveBuilder.Exception (ArchiveBuilder.Duplicate_entry _)
      | ArchiveBuilder.Exception (ArchiveBuilder.Duplicate_service _) ->
        ArchiveBuilder.close_noerr builder;
        Some "duplicate entry"
      | _ ->
        ArchiveBuilder.close_noerr builder;
        Some "cannot copy data")
  | None ->
      Some "cannot create archive"
