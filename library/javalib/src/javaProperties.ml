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

open Class'java'util'Properties

type t = _'Properties java_instance

let make ?(defaults = Java.null) () =
  if Java.is_not_null defaults then
    Java.make "Properties(Properties)" defaults
  else
    Java.make "Properties()" ()


(* Properties retrieval *)

let get_property props key =
  let res = Java.call "Properties.getProperty(String)" props key in
  if Java.is_not_null res then
    Some res
  else
    None

let get_property_default props key def =
  Java.call "Properties.getProperty(String,String)" props key def

let string_property_names props =
  let set = Java.call "Properties.stringPropertyNames()" props in
  let res = ref [] in
  let it = Java.call "java.util.Set.iterator()" set in
  while Java.call "java.util.Iterator.hasNext()" it do
    let elem = Java.call "java.util.Iterator.next()" it in
    res := (Java.cast "String" elem) :: !res
  done;
  !res


(* Properties modification *)

let set_property props key value =
  let res = Java.call "Properties.setProperty(String,String)" props key value in
  if Java.instanceof "String" res then
    Some (Java.cast "String" res)
  else
    None


(* I/O operations *)

let load props str =
  Java.call "Properties.load(java.io.InputStream)" props str

let load_from_xml props str =
  Java.call "Properties.loadFromXML(java.io.InputStream)" props str

let store props ?(comment = JavaString.null) str =
  Java.call "Properties.store(java.io.OutputStream,String)" props str comment

let store_to_xml props ?(comment = JavaString.null) str =
  Java.call "Properties.storeToXML(java.io.OutputStream,String)" props str comment


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
