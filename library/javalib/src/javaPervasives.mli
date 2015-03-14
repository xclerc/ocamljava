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

(** An initially-opened module for Java.

    The module is automatically opened when {k -java-extensions} is
    passed to the {k ocamljava} compiler. *)

external (|.) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Akin to [Pervasives.(@@)], with a different priority allowing to
    chain calls with parameters:
    {[
      Java.make "..."
      |> Java.call "..." |. p1 |. ... |. pn
      |> ...
    ]} *)

external (!@) : string -> java'lang'String java_instance =
  "ocamljava_javastring_of_string"
(** Synonym for {!JavaString.of_string}. *)

val (^^^) : java'lang'String java_instance -> java'lang'String java_instance -> java'lang'String java_instance
(** Concatenation of Java strings. *)
