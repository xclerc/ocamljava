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


(* Instance creation *)

open Class'java'io'BufferedInputStream
open Class'java'io'BufferedOutputStream
open Class'java'io'FileInputStream
open Class'java'io'FileOutputStream
open Class'java'io'InputStream
open Class'java'io'OutputStream

let open_in ?(buffered = true) str =
  let res = Java.make "FileInputStream(String)" str in
  if buffered then
    Java.make "BufferedInputStream(InputStream)" res
    |> Java.cast "InputStream"
  else
    res
    |> Java.cast "InputStream"

let open_out ?(buffered = true) str =
  let res = Java.make "FileOutputStream(String)" str in
  if buffered then
    Java.make "BufferedOutputStream(OutputStream)" res
    |> Java.cast "OutputStream"
  else
    res
    |> Java.cast "OutputStream"


(* Closing *)

let close_in str =
  Java.call "InputStream.close()" str

let close_in_noerr str =
  try
    Java.call "InputStream.close()" str
  with _ ->
    ()

let close_out str =
  Java.call "OutputStream.close()" str

let close_out_noerr str =
  try
    Java.call "OutputStream.close()" str
  with _ ->
    ()


(* Conversions *)

external in_channel_of_input_stream : java'io'InputStream java_extends -> in_channel =
  "ocamljava_in_channel_of_input_stream"

external out_channel_of_output_stream : java'io'OutputStream java_extends -> out_channel =
  "ocamljava_out_channel_of_output_stream"

external input_stream_of_in_channel : in_channel -> java'io'InputStream java_instance =
  "ocamljava_input_stream_of_in_channel"

external output_stream_of_out_channel : out_channel -> java'io'OutputStream java_instance =
  "ocamljava_output_stream_of_out_channel"
