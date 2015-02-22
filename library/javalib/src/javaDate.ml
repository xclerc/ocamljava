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

type t = java'util'Date java_instance

let now () =
  Java.make "java.util.Date()" ()

let make ms =
  Java.make "java.util.Date(long)" ms


(* Properties *)

let get_time date =
  Java.call "java.util.Date.getTime()" date

let after date when_ =
  Java.call "java.util.Date.after(java.util.Date)" date when_

let before date when_ =
  Java.call "java.util.Date.before(java.util.Date)" date when_

let compare_to date1 date2 =
  Java.call "java.util.Date.compareTo(java.util.Date)" date1 date2
  |> Int32.to_int


(* Conversions *)

type format = java'text'DateFormat java_instance

type format_style =
  | Full
  | Long
  | Medium
  | Short

let java_int_of_format_style = function
  | Full   -> Java.get "java.text.DateFormat.FULL"   ()
  | Long   -> Java.get "java.text.DateFormat.LONG"   ()
  | Medium -> Java.get "java.text.DateFormat.MEDIUM" ()
  | Short  -> Java.get "java.text.DateFormat.SHORT"  ()

let make_date_format ?(date_style = Medium) ?(locale = JavaLocale.null) ?(time_zone = JavaTimeZone.null) () =
  let res =
    if JavaLocale.is_not_null locale then
      Java.call "java.text.DateFormat.getDateInstance(int,java.util.Locale)"
        (java_int_of_format_style date_style)
        locale
    else
      Java.call "java.text.DateFormat.getDateInstance(int)"
        (java_int_of_format_style date_style) in
  if JavaTimeZone.is_not_null time_zone then
    Java.chain "java.text.DateFormat.setTimeZone(java.util.TimeZone)"
      res
      time_zone
  else
    res

let make_date_time_format ?(date_style = Medium) ?(time_style = Medium) ?(locale = JavaLocale.null) ?(time_zone = JavaTimeZone.null) () =
  let res =
    if JavaLocale.is_not_null locale then
      Java.call "java.text.DateFormat.getDateTimeInstance(int,int,java.util.Locale)"
        (java_int_of_format_style date_style)
        (java_int_of_format_style time_style)
        locale
    else
      Java.call "java.text.DateFormat.getDateTimeInstance(int,int)"
        (java_int_of_format_style date_style)
        (java_int_of_format_style time_style) in
  if JavaTimeZone.is_not_null time_zone then
    Java.chain "java.text.DateFormat.setTimeZone(java.util.TimeZone)"
      res
      time_zone
  else
    res

let make_simple_format ?(pattern = JavaString.null) ?(locale = JavaLocale.null) ?(time_zone = JavaTimeZone.null) () =
  let res =
    if JavaString.is_not_null pattern then begin
      if JavaLocale.is_not_null locale then
        Java.make "java.text.SimpleDateFormat(String,java.util.Locale)"
          pattern locale
      else
        Java.make "java.text.SimpleDateFormat(String)"
          pattern
    end else
      Java.make "java.text.SimpleDateFormat()" () in
  if JavaTimeZone.is_not_null time_zone then
    Java.chain "java.text.DateFormat.setTimeZone(java.util.TimeZone)"
      res
      time_zone
  else
    res
    |> Java.cast "java.text.DateFormat"

let to_string fmt date =
  Java.call "java.text.SimpleDateFormat.format(Object)"
    fmt
    date

let of_string fmt str =
  Java.call "java.text.SimpleDateFormat.parse(String)"
    fmt
    str


(* Null value *)

external null : unit -> 'a java_instance =
  "java null"

let null = null ()

external is_null : 'a java_instance -> bool =
  "java is_null"

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"


(* Miscellaneous *)

let wrap x =
  if is_null x then
    None
  else
    Some x

let unwrap = function
  | Some x -> x
  | None   -> null
