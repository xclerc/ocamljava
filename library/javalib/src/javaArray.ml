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


type (_, _, _) t =
  | Boolean_array :    bool java_boolean_array -> (bool,  int32, bool java_boolean_array) t
  | Byte_array :       int java_byte_array ->     (int,   int32, int java_byte_array) t
  | Char_array :       int java_char_array ->     (int,   int32, int java_char_array) t
  | Double_array :     float java_double_array -> (float, int32, float java_double_array) t
  | Float_array :      float java_float_array ->  (float, int32, float java_float_array) t
  | Int_array :        int32 java_int_array ->    (int32, int32, int32 java_int_array) t
  | Long_array :       int64 java_long_array ->   (int64, int32, int64 java_long_array) t
  | Short_array :      int java_short_array ->    (int,   int32, int java_short_array) t
  | Reference_array :  'a java_reference_array -> ('a,    int32, 'a java_reference_array) t
  | Boolean_array2 :   bool java_boolean_array java_reference_array -> (bool,  int32 * int32, bool java_boolean_array java_reference_array) t
  | Byte_array2 :      int java_byte_array java_reference_array ->     (int,   int32 * int32, int java_byte_array java_reference_array) t
  | Char_array2 :      int java_char_array java_reference_array ->     (int,   int32 * int32, int java_char_array java_reference_array) t
  | Double_array2 :    float java_double_array java_reference_array -> (float, int32 * int32, float java_double_array java_reference_array) t
  | Float_array2 :     float java_float_array java_reference_array ->  (float, int32 * int32, float java_float_array java_reference_array) t
  | Int_array2 :       int32 java_int_array java_reference_array ->    (int32, int32 * int32, int32 java_int_array java_reference_array) t
  | Long_array2 :      int64 java_long_array java_reference_array ->   (int64, int32 * int32, int64 java_long_array java_reference_array) t
  | Short_array2 :     int java_short_array java_reference_array ->    (int,   int32 * int32, int java_short_array java_reference_array) t
  | Reference_array2 : 'a java_reference_array java_reference_array -> ('a,    int32 * int32, 'a java_reference_array java_reference_array) t

let wrap_boolean_array a = Boolean_array a

let wrap_byte_array a = Byte_array a

let wrap_char_array a = Char_array a

let wrap_double_array a = Double_array a

let wrap_float_array a = Float_array a

let wrap_int_array a = Int_array a

let wrap_long_array a = Long_array a

let wrap_short_array a = Short_array a

let wrap_reference_array a = Reference_array a

let wrap_boolean_array2 a = Boolean_array2 a

let wrap_byte_array2 a = Byte_array2 a

let wrap_char_array2 a = Char_array2 a

let wrap_double_array2 a = Double_array2 a

let wrap_float_array2 a = Float_array2 a

let wrap_int_array2 a = Int_array2 a

let wrap_long_array2 a = Long_array2 a

let wrap_short_array2 a = Short_array2 a

let wrap_reference_array2 a = Reference_array2 a

let length : type e i r . (e, i, r) t -> int32 = fun a ->
  match a with
  | Boolean_array a -> JavaBooleanArray.length a
  | Byte_array a -> JavaByteArray.length a
  | Char_array a -> JavaCharArray.length a
  | Double_array a -> JavaDoubleArray.length a
  | Float_array a -> JavaFloatArray.length a
  | Int_array a -> JavaIntArray.length a
  | Long_array a -> JavaLongArray.length a
  | Short_array a -> JavaShortArray.length a
  | Reference_array a -> JavaReferenceArray.length a
  | Boolean_array2 a -> JavaReferenceArray.length a
  | Byte_array2 a -> JavaReferenceArray.length a
  | Char_array2 a -> JavaReferenceArray.length a
  | Double_array2 a -> JavaReferenceArray.length a
  | Float_array2 a -> JavaReferenceArray.length a
  | Int_array2 a -> JavaReferenceArray.length a
  | Long_array2 a -> JavaReferenceArray.length a
  | Short_array2 a -> JavaReferenceArray.length a
  | Reference_array2 a -> JavaReferenceArray.length a

let length_sub : type e r . (e, int32 * int32, r) t -> int32 -> int32 = fun a i ->
  match a with
  | Boolean_array2 a -> JavaBooleanArray.length (JavaReferenceArray.get a i)
  | Byte_array2 a -> JavaByteArray.length (JavaReferenceArray.get a i)
  | Char_array2 a -> JavaCharArray.length (JavaReferenceArray.get a i)
  | Double_array2 a -> JavaDoubleArray.length (JavaReferenceArray.get a i)
  | Float_array2 a -> JavaFloatArray.length (JavaReferenceArray.get a i)
  | Int_array2 a -> JavaIntArray.length (JavaReferenceArray.get a i)
  | Long_array2 a -> JavaLongArray.length (JavaReferenceArray.get a i)
  | Short_array2 a -> JavaShortArray.length (JavaReferenceArray.get a i)
  | Reference_array2 a -> JavaReferenceArray.length (JavaReferenceArray.get a i)

let get : type e i r . (e, i, r) t -> i -> e = fun a i ->
  match a with
  | Boolean_array a -> JavaBooleanArray.get a i
  | Byte_array a -> JavaByteArray.get a i
  | Char_array a -> JavaCharArray.get a i
  | Double_array a -> JavaDoubleArray.get a i
  | Float_array a -> JavaFloatArray.get a i
  | Int_array a -> JavaIntArray.get a i
  | Long_array a -> JavaLongArray.get a i
  | Short_array a -> JavaShortArray.get a i
  | Reference_array a -> JavaReferenceArray.get a i
  | Boolean_array2 a -> JavaBooleanArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Byte_array2 a -> JavaByteArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Char_array2 a -> JavaCharArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Double_array2 a -> JavaDoubleArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Float_array2 a -> JavaFloatArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Int_array2 a -> JavaIntArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Long_array2 a -> JavaLongArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Short_array2 a -> JavaShortArray.get (JavaReferenceArray.get a (fst i)) (snd i)
  | Reference_array2 a -> JavaReferenceArray.get (JavaReferenceArray.get a (fst i)) (snd i)

let set : type e i r . (e, i, r) t -> i -> e -> unit = fun a i x ->
  match a with
  | Boolean_array a -> JavaBooleanArray.set a i x
  | Byte_array a -> JavaByteArray.set a i x
  | Char_array a -> JavaCharArray.set a i x
  | Double_array a -> JavaDoubleArray.set a i x
  | Float_array a -> JavaFloatArray.set a i x
  | Int_array a -> JavaIntArray.set a i x
  | Long_array a -> JavaLongArray.set a i x
  | Short_array a -> JavaShortArray.set a i x
  | Reference_array a -> JavaReferenceArray.set a i x
  | Boolean_array2 a -> JavaBooleanArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Byte_array2 a -> JavaByteArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Char_array2 a -> JavaCharArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Double_array2 a -> JavaDoubleArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Float_array2 a -> JavaFloatArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Int_array2 a -> JavaIntArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Long_array2 a -> JavaLongArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Short_array2 a -> JavaShortArray.set (JavaReferenceArray.get a (fst i)) (snd i) x
  | Reference_array2 a -> JavaReferenceArray.set (JavaReferenceArray.get a (fst i)) (snd i) x

let iter2 = fun apply f a ->
  let len = JavaReferenceArray.length a in
  let i = ref 0l in
  while !i < len do
    apply f (JavaReferenceArray.get a !i);
    i := Int32.succ !i
  done

let iter : type e i r . (e -> unit) -> (e, i, r) t -> unit = fun f a ->
  match a with
  | Boolean_array a -> JavaBooleanArray.iter f a
  | Byte_array a -> JavaByteArray.iter f a
  | Char_array a -> JavaCharArray.iter f a
  | Double_array a -> JavaDoubleArray.iter f a
  | Float_array a -> JavaFloatArray.iter f a
  | Int_array a -> JavaIntArray.iter f a
  | Long_array a -> JavaLongArray.iter f a
  | Short_array a -> JavaShortArray.iter f a
  | Reference_array a -> JavaReferenceArray.iter f a
  | Boolean_array2 a -> iter2 JavaBooleanArray.iter f a
  | Byte_array2 a -> iter2 JavaByteArray.iter f a
  | Char_array2 a -> iter2 JavaCharArray.iter f a
  | Double_array2 a -> iter2 JavaDoubleArray.iter f a
  | Float_array2 a -> iter2 JavaFloatArray.iter f a
  | Int_array2 a -> iter2 JavaIntArray.iter f a
  | Long_array2 a -> iter2 JavaLongArray.iter f a
  | Short_array2 a -> iter2 JavaShortArray.iter f a
  | Reference_array2 a -> iter2 JavaReferenceArray.iter f a

let iteri2 = fun apply f a ->
  let len = JavaReferenceArray.length a in
  let i = ref 0l in
  while !i < len do
    apply
      (fun j e -> f (!i, j) e)
      (JavaReferenceArray.get a !i);
    i := Int32.succ !i
  done

let iteri : type e i r . (i -> e -> unit) -> (e, i, r) t -> unit = fun f a ->
  match a with
  | Boolean_array a -> JavaBooleanArray.iteri f a
  | Byte_array a -> JavaByteArray.iteri f a
  | Char_array a -> JavaCharArray.iteri f a
  | Double_array a -> JavaDoubleArray.iteri f a
  | Float_array a -> JavaFloatArray.iteri f a
  | Int_array a -> JavaIntArray.iteri f a
  | Long_array a -> JavaLongArray.iteri f a
  | Short_array a -> JavaShortArray.iteri f a
  | Reference_array a -> JavaReferenceArray.iteri f a
  | Boolean_array2 a -> iteri2 JavaBooleanArray.iteri f a
  | Byte_array2 a -> iteri2 JavaByteArray.iteri f a
  | Char_array2 a -> iteri2 JavaCharArray.iteri f a
  | Double_array2 a -> iteri2 JavaDoubleArray.iteri f a
  | Float_array2 a -> iteri2 JavaFloatArray.iteri f a
  | Int_array2 a -> iteri2 JavaIntArray.iteri f a
  | Long_array2 a -> iteri2 JavaLongArray.iteri f a
  | Short_array2 a -> iteri2 JavaShortArray.iteri f a
  | Reference_array2 a -> iteri2 JavaReferenceArray.iteri f a

let is_null : type e i r . (e, i, r) t -> bool = fun a ->
  match a with
  | Boolean_array a -> JavaBooleanArray.is_null a
  | Byte_array a -> JavaByteArray.is_null a
  | Char_array a -> JavaCharArray.is_null a
  | Double_array a -> JavaDoubleArray.is_null a
  | Float_array a -> JavaFloatArray.is_null a
  | Int_array a -> JavaIntArray.is_null a
  | Long_array a -> JavaLongArray.is_null a
  | Short_array a -> JavaShortArray.is_null a
  | Reference_array a -> JavaReferenceArray.is_null a
  | Boolean_array2 a -> JavaReferenceArray.is_null a
  | Byte_array2 a -> JavaReferenceArray.is_null a
  | Char_array2 a -> JavaReferenceArray.is_null a
  | Double_array2 a -> JavaReferenceArray.is_null a
  | Float_array2 a -> JavaReferenceArray.is_null a
  | Int_array2 a -> JavaReferenceArray.is_null a
  | Long_array2 a -> JavaReferenceArray.is_null a
  | Short_array2 a -> JavaReferenceArray.is_null a
  | Reference_array2 a -> JavaReferenceArray.is_null a

let is_not_null : type e i r . (e, i, r) t -> bool = fun a ->
  match a with
  | Boolean_array a -> JavaBooleanArray.is_not_null a
  | Byte_array a -> JavaByteArray.is_not_null a
  | Char_array a -> JavaCharArray.is_not_null a
  | Double_array a -> JavaDoubleArray.is_not_null a
  | Float_array a -> JavaFloatArray.is_not_null a
  | Int_array a -> JavaIntArray.is_not_null a
  | Long_array a -> JavaLongArray.is_not_null a
  | Short_array a -> JavaShortArray.is_not_null a
  | Reference_array a -> JavaReferenceArray.is_not_null a
  | Boolean_array2 a -> JavaReferenceArray.is_not_null a
  | Byte_array2 a -> JavaReferenceArray.is_not_null a
  | Char_array2 a -> JavaReferenceArray.is_not_null a
  | Double_array2 a -> JavaReferenceArray.is_not_null a
  | Float_array2 a -> JavaReferenceArray.is_not_null a
  | Int_array2 a -> JavaReferenceArray.is_not_null a
  | Long_array2 a -> JavaReferenceArray.is_not_null a
  | Short_array2 a -> JavaReferenceArray.is_not_null a
  | Reference_array2 a -> JavaReferenceArray.is_not_null a

let wrap x =
  if is_null x then
    None
  else
    Some x

let wrapped : type e i r . (e, i, r) t -> r = fun a ->
  match a with
  | Boolean_array a -> a
  | Byte_array a -> a
  | Char_array a -> a
  | Double_array a -> a
  | Float_array a -> a
  | Int_array a -> a
  | Long_array a -> a
  | Short_array a -> a
  | Reference_array a -> a
  | Boolean_array2 a -> a
  | Byte_array2 a -> a
  | Char_array2 a -> a
  | Double_array2 a -> a
  | Float_array2 a -> a
  | Int_array2 a -> a
  | Long_array2 a -> a
  | Short_array2 a -> a
  | Reference_array2 a -> a
