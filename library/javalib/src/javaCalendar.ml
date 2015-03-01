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

open Class'java'util'Calendar
open Class'java'util'Date
open Class'java'util'Locale
open Class'java'util'TimeZone

type t = _'Calendar java_instance

let now ?(locale = JavaLocale.null) ?(zone = JavaTimeZone.null) () =
  if JavaLocale.is_null locale then begin
    if JavaTimeZone.is_null zone then
      Java.call "Calendar.getInstance()" ()
    else
      Java.call "Calendar.getInstance(TimeZone)" zone
  end else begin
    if JavaTimeZone.is_null zone then
      Java.call "Calendar.getInstance(Locale)" locale
    else
      Java.call "Calendar.getInstance(TimeZone,Locale)"
        zone locale
  end


(* Fields *)

type am_pm =
  | AM
  | PM

let java_int_of_am_pm = function
  | AM -> Java.get "Calendar.AM" ()
  | PM -> Java.get "Calendar.PM" ()

let am_pm_of_java_int = function
  | 0l -> AM
  | 1l -> PM
  | _  -> invalid_arg "JavaCalendar.am_pm_of_java_int"

type month =
  | January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  | Undecimber

let java_int_of_month = function
  | January    -> Java.get "Calendar.JANUARY"    ()
  | February   -> Java.get "Calendar.FEBRUARY"   ()
  | March      -> Java.get "Calendar.MARCH"      ()
  | April      -> Java.get "Calendar.APRIL"      ()
  | May        -> Java.get "Calendar.MAY"        ()
  | June       -> Java.get "Calendar.JUNE"       ()
  | July       -> Java.get "Calendar.JULY"       ()
  | August     -> Java.get "Calendar.AUGUST"     ()
  | September  -> Java.get "Calendar.SEPTEMBER"  ()
  | October    -> Java.get "Calendar.OCTOBER"    ()
  | November   -> Java.get "Calendar.NOVEMBER"   ()
  | December   -> Java.get "Calendar.DECEMBER"   ()
  | Undecimber -> Java.get "Calendar.UNDECIMBER" ()

let month_of_java_int = function
  | 0l  -> January
  | 1l  -> February
  | 2l  -> March
  | 3l  -> April
  | 4l  -> May
  | 5l  -> June
  | 6l  -> July
  | 7l  -> August
  | 8l  -> September
  | 9l  -> October
  | 10l -> November
  | 11l -> December
  | 12l -> Undecimber
  | _  -> invalid_arg "JavaCalendar.month_of_java_int"

type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

let java_int_of_day = function
  | Monday    -> Java.get "Calendar.MONDAY"    ()
  | Tuesday   -> Java.get "Calendar.TUESDAY"   ()
  | Wednesday -> Java.get "Calendar.WEDNESDAY" ()
  | Thursday  -> Java.get "Calendar.THURSDAY"  ()
  | Friday    -> Java.get "Calendar.FRIDAY"    ()
  | Saturday  -> Java.get "Calendar.SATURDAY"  ()
  | Sunday    -> Java.get "Calendar.SUNDAY"    ()

let day_of_java_int = function
  | 1l -> Sunday
  | 2l -> Monday
  | 3l -> Tuesday
  | 4l -> Wednesday
  | 5l -> Thursday
  | 6l -> Friday
  | 7l -> Saturday
  | _  -> invalid_arg "JavaCalendar.day_of_java_int"

type _ field =
  | Era                  : java_int field
  | Year                 : java_int field
  | Month                : month    field
  | Week_of_month        : java_int field
  | Week_of_year         : java_int field
  | Date                 : java_int field
  | Day_of_month         : java_int field
  | Day_of_week          : day      field
  | Day_of_week_in_month : java_int field
  | Day_of_year          : java_int field
  | AM_PM                : am_pm    field
  | Hour                 : java_int field
  | Hour_of_day          : java_int field
  | Minute               : java_int field
  | Second               : java_int field
  | Millisecond          : java_int field
  | Dst_offset           : java_int field
  | Zone_offset          : java_int field

let java_int_of_field : type a . a field -> java_int = function
  | Era                  -> Java.get "Calendar.ERA"                  ()
  | Year                 -> Java.get "Calendar.YEAR"                 ()
  | Month                -> Java.get "Calendar.MONTH"                ()
  | Week_of_month        -> Java.get "Calendar.WEEK_OF_MONTH"        ()
  | Week_of_year         -> Java.get "Calendar.WEEK_OF_YEAR"         ()
  | Date                 -> Java.get "Calendar.DATE"                 ()
  | Day_of_month         -> Java.get "Calendar.DAY_OF_MONTH"         ()
  | Day_of_week          -> Java.get "Calendar.DAY_OF_WEEK"          ()
  | Day_of_week_in_month -> Java.get "Calendar.DAY_OF_WEEK_IN_MONTH" ()
  | Day_of_year          -> Java.get "Calendar.DAY_OF_YEAR"          ()
  | AM_PM                -> Java.get "Calendar.AM_PM"                ()
  | Hour                 -> Java.get "Calendar.HOUR"                 ()
  | Hour_of_day          -> Java.get "Calendar.HOUR_OF_DAY"          ()
  | Minute               -> Java.get "Calendar.MINUTE"               ()
  | Second               -> Java.get "Calendar.SECOND"               ()
  | Millisecond          -> Java.get "Calendar.MILLISECOND"          ()
  | Dst_offset           -> Java.get "Calendar.DST_OFFSET"           ()
  | Zone_offset          -> Java.get "Calendar.ZONE_OFFSET"          ()


(* Properties *)

let get_time cal =
  Java.call "Calendar.getTimeInMillis()" cal

let after cal when_ =
  Java.call "Calendar.after(Object)" cal when_

let before cal when_ =
  Java.call "Calendar.before(Object)" cal when_

let compare_to cal1 cal2 =
  Java.call "Calendar.compareTo(Calendar)" cal1 cal2
  |> Int32.to_int

let get : type a . t -> a field -> a = fun cal f ->
  let f' = java_int_of_field f in
  match f with
  | Era                  -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Year                 -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Month                -> (Java.call "Calendar.get(int)" cal f' |> month_of_java_int)
  | Week_of_month        -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Week_of_year         -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Date                 -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Day_of_month         -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Day_of_week          -> (Java.call "Calendar.get(int)" cal f' |> day_of_java_int)
  | Day_of_week_in_month -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Day_of_year          -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | AM_PM                -> (Java.call "Calendar.get(int)" cal f' |> am_pm_of_java_int)
  | Hour                 -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Hour_of_day          -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Minute               -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Second               -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Millisecond          -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Dst_offset           -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)
  | Zone_offset          -> (Java.call "Calendar.get(int)" cal f' : java_int :> a)

let get_time_zone cal =
  Java.call "Calendar.getTimeZone()" cal


(* Creation of new calendar values *)

let copy cal =
  Java.call "Calendar.clone()" cal
  |> Java.cast "Calendar"

let add : type a . t -> a field -> java_int -> t = fun cal f delta ->
  let cal = copy cal in
  let f' = java_int_of_field f in
  match f with
  | Era                  -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Year                 -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Month                -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Week_of_month        -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Week_of_year         -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Date                 -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Day_of_month         -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Day_of_week          -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Day_of_week_in_month -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Day_of_year          -> Java.chain "Calendar.add(int,int)" cal f' delta
  | AM_PM                -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Hour                 -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Hour_of_day          -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Minute               -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Second               -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Millisecond          -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Dst_offset           -> Java.chain "Calendar.add(int,int)" cal f' delta
  | Zone_offset          -> Java.chain "Calendar.add(int,int)" cal f' delta

let set : type a . t -> a field -> a -> t = fun cal f v ->
  let cal = copy cal in
  let f' = java_int_of_field f in
  match f with
  | Era                  -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Year                 -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Month                -> Java.chain "Calendar.set(int,int)" cal f' (java_int_of_month v)
  | Week_of_month        -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Week_of_year         -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Date                 -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Day_of_month         -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Day_of_week          -> Java.chain "Calendar.set(int,int)" cal f' (java_int_of_day v)
  | Day_of_week_in_month -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Day_of_year          -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | AM_PM                -> Java.chain "Calendar.set(int,int)" cal f' (java_int_of_am_pm v)
  | Hour                 -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Hour_of_day          -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Minute               -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Second               -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Millisecond          -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Dst_offset           -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)
  | Zone_offset          -> Java.chain "Calendar.set(int,int)" cal f' (v : a :> java_int)

let clear cal f =
  copy cal
  |> Java.chain "Calendar.clear(int)" |. (java_int_of_field f)

let clear_all cal =
  copy cal
  |> Java.chain "Calendar.clear()"

let set_time_zone cal tz =
  copy cal
  |> Java.chain "Calendar.setTimeZone(TimeZone)" |. tz


(* Conversions *)

let to_date cal =
  Java.call "Calendar.getTime()" cal

let of_date date =
  now ()
  |> Java.chain "Calendar.setTime(Date)" |. date

let to_iso8601 cal =
  Java.call "javax.xml.bind.DatatypeConverter.printDateTime(Calendar)" cal

let of_iso8601 s =
  Java.call "javax.xml.bind.DatatypeConverter.parseDateTime(String)" s


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
