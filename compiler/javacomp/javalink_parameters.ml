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

module StringSet = Set.Make(String)

let compile params =
  let already_seen = ref StringSet.empty in
  params
  |> List.rev_map
      (fun str ->
        let name, value =
          try
            let idx = String.index str '=' in
            String.trim (String.sub str 0 idx),
            String.trim (String.sub str (succ idx) ((String.length str) - idx - 1))
          with Not_found ->
            let msg = Printf.sprintf "invalid runtime parameter argument %S" str in
            failwith msg in
        if StringSet.mem name !already_seen then
          let msg = Printf.sprintf "duplicate runtime parameter %S" name in
          failwith msg
        else
          already_seen := StringSet.add name !already_seen;
        let ensure_among l x =
          if not (List.mem x l) then
            let values = List.map (fun v -> Printf.sprintf "%S" v) l in
            let msg =
              Printf.sprintf "invalid value %S for parameter %S (possible values are: %s)"
                x
                name
                (String.concat ", " values) in
            failwith msg in
        let ensure_on_or_off x =
          ensure_among [ "on"; "off" ] x in
        begin match name with
        (* from org.ocamljava.runtime.parameters.CommonParameters *)
        | "backtrace"            -> ensure_on_or_off value
        | "stop-jvm"             -> ensure_on_or_off value
        | "awt"                  -> ensure_on_or_off value
        | "javax-sound"          -> ensure_on_or_off value
        | "os"                   -> ensure_among [ "auto"; "Unix"; "Cygwin"; "Win32"; "Java" ] value
        | "unix-emulation"       -> ensure_on_or_off value
        | "embedded"             -> ensure_on_or_off value
        | "embedded-base"        -> ()
        | "runtime-lock"         -> ensure_on_or_off value
        (* from org.ocamljava.runtime.parameters.NativeParameters *)
        | "simplified-backtrace" -> ensure_on_or_off value
        | _ ->
            let msg = Printf.sprintf "invalid runtime parameter name %S" name in
            failwith msg
        end;
        Printf.sprintf "%s=%s" name value
        |> UTF8.of_string)
  |> UTF8.concat_sep (UTF8.of_string "\n")
  |> UTF8.to_latin1
