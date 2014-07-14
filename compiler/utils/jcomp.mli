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


(** Compilation of Java source files. *)

val compile_file : string -> int
(** Compiles the passed Java file through a Java compiler, using the
    values passed by {i -javac} (compiler), {i -jopt} (compiler options),
    and {i -cp}/{i -classpath} (compilation classpath) command-line
    switches.

    The path to the runtime support jar is prepended to the classpath.

    Returns the exit code of the process running the Java compiler. *)
