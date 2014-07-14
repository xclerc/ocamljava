(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Conversions between Java streams and OCaml channels. *)

external in_channel_of_input_stream : java'io'InputStream java_instance -> in_channel =
  "ocamljava_in_channel_of_input_stream"
(** [in_channel_of_input_stream is] converts the input stream [is] into a
    new input channel. The returned channel is added to the list of
    opened channels. *)

external out_channel_of_output_stream : java'io'OutputStream java_instance -> out_channel =
  "ocamljava_out_channel_of_output_stream"
(** [out_channel_of_output_stream os] converts the output stream [os]
    into a new output channel. The returned channel is added to the list
    of opened channels. *)

external input_stream_of_in_channel : in_channel -> java'io'InputStream java_instance =
  "ocamljava_input_stream_of_in_channel"
(** [input_stream_of_in_channel ic] converts the input channel [ic] into
    a new input stream. *)

external output_stream_of_out_channel : out_channel -> java'io'OutputStream java_instance =
  "ocamljava_output_stream_of_out_channel"
(** [output_stream_of_out_channel oc] converts the output channel [oc]
    into a new output stream. *)
