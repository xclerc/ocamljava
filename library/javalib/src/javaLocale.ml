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


(* Instance creation *)

type t = java'util'Locale java_instance
(** The type of locales. *)

let make ?(country = JavaString.null) ?(variant = JavaString.null) ~language =
  if JavaString.is_null country then
    Java.make "java.util.Locale(String)" language
  else if JavaString.is_null variant then
    Java.make "java.util.Locale(String,String)" language country
  else
    Java.make "java.util.Locale(String,String,String)" language country variant

let get_available_locales () =
  let arr = Java.call "java.util.Locale.getAvailableLocales()" () in
  let i = ref @@ JavaReferenceArray.length arr in
  let res = ref [] in
  while !i >= 0l do
    res := (JavaReferenceArray.get arr !i) :: !res
  done;
  !res

let get_default () =
  Java.call "java.util.Locale.getDefault()" ()

let set_default loc =
  Java.call "java.util.Locale.setDefault(java.util.Locale)" loc


(* Properties *)

let get_display_name loc =
  Java.call "java.util.Locale.getDisplayName()" loc

let get_language loc =
  Java.call "java.util.Locale.getLanguage()" loc

let get_display_language loc =
  Java.call "java.util.Locale.getDisplayLanguage()" loc

let get_iso3_language loc =
  Java.call "java.util.Locale.getISO3Language()" loc

let get_country loc =
  Java.call "java.util.Locale.getCountry()" loc

let get_display_country loc =
  Java.call "java.util.Locale.getDisplayCountry()" loc

let get_iso3_country loc =
  Java.call "java.util.Locale.getISO3Country()" loc

let get_variant loc =
  Java.call "java.util.Locale.getVariant()" loc

let get_display_variant loc =
  Java.call "java.util.Locale.getDisplayVariant()" loc

let get_script loc =
  Java.call "java.util.Locale.getScript()" loc

let get_display_script loc =
  Java.call "java.util.Locale.getDisplayScript()" loc

let to_language_tag loc =
  Java.call "java.util.Locale.toLanguageTag()" loc


(* Predefined locales *)

(* Languages *)

let chinese () =
  Java.get "java.util.Locale.CHINESE" ()

let english () =
  Java.get "java.util.Locale.ENGLISH" ()

let french () =
  Java.get "java.util.Locale.FRENCH" ()

let german () =
  Java.get "java.util.Locale.GERMAN" ()

let italian () =
  Java.get "java.util.Locale.ITALIAN" ()

let japanese () =
  Java.get "java.util.Locale.JAPANESE" ()

let korean () =
  Java.get "java.util.Locale.KOREAN" ()

let simplified_chinese () =
  Java.get "java.util.Locale.SIMPLIFIED_CHINESE" ()

let traditional_chinese () =
  Java.get "java.util.Locale.TRADITIONAL_CHINESE" ()

(* countries *)

let canada () =
  Java.get "java.util.Locale.CANADA" ()

let canada_french () =
  Java.get "java.util.Locale.CANADA_FRENCH" ()

let china () =
  Java.get "java.util.Locale.CHINA" ()

let france () =
  Java.get "java.util.Locale.FRANCE" ()

let germany () =
  Java.get "java.util.Locale.GERMANY" ()

let italy () =
  Java.get "java.util.Locale.ITALY" ()

let japan () =
  Java.get "java.util.Locale.JAPAN" ()

let korea () =
  Java.get "java.util.Locale.KOREA" ()

let prc () =
  Java.get "java.util.Locale.PRC" ()

let taiwan () =
  Java.get "java.util.Locale.TAIWAN" ()

let uk () =
  Java.get "java.util.Locale.UK" ()

let us () =
  Java.get "java.util.Locale.US" ()


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
