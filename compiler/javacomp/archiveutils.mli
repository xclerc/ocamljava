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

(**  Utility functions over [BaristaLibrary.ArchiveBuilder.t]. *)


val open_builder : string -> BaristaLibrary.ArchiveBuilder.t
(** Creates a buider for the given path. *)

val add_entry : BaristaLibrary.ArchiveBuilder.t -> string -> BaristaLibrary.Bytes.t -> unit
(** [add_entry builder name bytes] adds to [builder] an entry named
    [name] whose data is [bytes]. *)

val add_entry_from_file : BaristaLibrary.ArchiveBuilder.t -> string -> string -> unit
(** [add_entry_from_file builder name file] adds to [builder] an entry
    named [name] whose data is read from [file]. *)

val close : BaristaLibrary.ArchiveBuilder.t -> unit
(** Closes the passed builder, possibly raising an exception. *)

val close_noerr : BaristaLibrary.ArchiveBuilder.t -> unit
(** Closes the passed builder, discarding any exception. *)

val copy_entries_list : string list -> string -> string option
(** [copy_entries_list archive_list archive] creates the archive
    [archive] and copies to it all entries for the archives in
    [archive_list]. *)
