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

(** Some constants and utility functions for bytecode compilation. *)


(** {6 Name functions} *)

val normalize_function_name : string -> BaristaLibrary.UTF8.t
(** Normalizes the passed function name such that it is a correct
    Java method name. *)

val make_class : string -> BaristaLibrary.Name.for_class
(** Constructs a class name from the passed string. *)

val make_field : string -> BaristaLibrary.Name.for_field
(** Constructs a field name from the passed string. *)

val make_method : string -> BaristaLibrary.Name.for_method
(** Constructs a method name from the passed string,
    the name being normalized through [normalize_function_name]. *)


(** {6 Class and interface constants} *)

val class_AbstractAwtApplet : BaristaLibrary.Name.for_class
val class_AbstractGraphicsApplet : BaristaLibrary.Name.for_class
val class_AbstractNativeRunner : BaristaLibrary.Name.for_class
val class_AbstractSwingApplet : BaristaLibrary.Name.for_class
val class_BlockValue : BaristaLibrary.Name.for_class
val class_Class : BaristaLibrary.Name.for_class
val class_CompiledModule : BaristaLibrary.Name.for_class
val class_Constants : BaristaLibrary.Name.for_class
val class_EntryPoint : BaristaLibrary.Name.for_class
val class_Fail : BaristaLibrary.Name.for_class
val class_FailException : BaristaLibrary.Name.for_class
val class_Global : BaristaLibrary.Name.for_class
val class_GlobalUses : BaristaLibrary.Name.for_class
val class_GlobalValue : BaristaLibrary.Name.for_class
val class_InputStream : BaristaLibrary.Name.for_class
val class_Int32Value : BaristaLibrary.Name.for_class
val class_Int64Value : BaristaLibrary.Name.for_class
val class_LongValue : BaristaLibrary.Name.for_class
val class_Map : BaristaLibrary.Name.for_class
val class_Math : BaristaLibrary.Name.for_class
val class_MethodHandle : BaristaLibrary.Name.for_class
val class_MethodMapping : BaristaLibrary.Name.for_class
val class_NativeIntValue : BaristaLibrary.Name.for_class
val class_Object : BaristaLibrary.Name.for_class
val class_Parameters : BaristaLibrary.Name.for_class
val class_PredefinedExceptions : BaristaLibrary.Name.for_class
val class_Primitive : BaristaLibrary.Name.for_class
val class_PrimitiveProvider : BaristaLibrary.Name.for_class
val class_PrintStream : BaristaLibrary.Name.for_class
val class_String : BaristaLibrary.Name.for_class
val class_System : BaristaLibrary.Name.for_class
val class_ThreadLocal : BaristaLibrary.Name.for_class
val class_Value : BaristaLibrary.Name.for_class

val interface_NativeParameters : BaristaLibrary.Name.for_class
val interface_OCamlJavaModule : BaristaLibrary.Name.for_class


(** {6 Field constants} *)

val field_System_err : Instrtree.t
val field_System_in : Instrtree.t
val field_System_out : Instrtree.t

val field_Value_EIGHT : Instrtree.t
val field_Value_FALSE : Instrtree.t
val field_Value_FIVE : Instrtree.t
val field_Value_FOUR : Instrtree.t
val field_Value_MINUS_ONE : Instrtree.t
val field_Value_MINUS_TWO : Instrtree.t
val field_Value_ONE : Instrtree.t
val field_Value_SEVEN : Instrtree.t
val field_Value_SIX : Instrtree.t
val field_Value_THREE : Instrtree.t
val field_Value_TRUE : Instrtree.t
val field_Value_TWO : Instrtree.t
val field_Value_UNIT : Instrtree.t
val field_Value_ZERO : Instrtree.t


(** {6 Method constants} *)

val cstr_AbstractAwtApplet : Instrtree.t
val cstr_AbstractGraphicsApplet : Instrtree.t
val cstr_AbstractNativeRunner : Instrtree.t
val cstr_AbstractSwingApplet : Instrtree.t
val cstr_GlobalValue : Instrtree.t
val cstr_MethodMapping : Instrtree.t
val cstr_copy_AbstractNativeRunner : Instrtree.t

val meth_abs : Instrtree.t
val meth_add : Instrtree.t
val meth_addint : Instrtree.t
val meth_apply : Instrtree.t
val meth_apply1 : Instrtree.t
val meth_apply2 : Instrtree.t
val meth_apply3 : Instrtree.t
val meth_apply4 : Instrtree.t
val meth_apply5 : Instrtree.t
val meth_apply6 : Instrtree.t
val meth_apply7 : Instrtree.t
val meth_apply8 : Instrtree.t
val meth_arrayLength : Instrtree.t
val meth_asCustom : Instrtree.t
val meth_asDouble : Instrtree.t
val meth_asInt32 : Instrtree.t
val meth_asInt64 : Instrtree.t
val meth_asLong : Instrtree.t
val meth_asNativeInt : Instrtree.t
val meth_asString : Instrtree.t
val meth_asValue : Instrtree.t
val meth_asrint : Instrtree.t
val meth_bitvect_test : Instrtree.t
val meth_caml_array_get : Instrtree.t
val meth_caml_array_get_addr : Instrtree.t
val meth_caml_array_get_float : Instrtree.t
val meth_caml_array_get_int : Instrtree.t
val meth_caml_array_set : Instrtree.t
val meth_caml_array_set_addr : Instrtree.t
val meth_caml_array_set_float : Instrtree.t
val meth_caml_array_set_int : Instrtree.t
val meth_caml_array_unsafe_get : Instrtree.t
val meth_caml_array_unsafe_set : Instrtree.t
val meth_caml_make_array : Instrtree.t
val meth_caml_string_get : Instrtree.t
val meth_caml_string_set : Instrtree.t
val meth_checkSignals : Instrtree.t
val meth_constantsStorage : Instrtree.t
val meth_createBlock : Instrtree.t
val meth_createClosure : Instrtree.t
val meth_createClosure1 : Instrtree.t
val meth_createClosureN : Instrtree.t
val meth_createDouble : Instrtree.t
val meth_createDoubleArray : Instrtree.t
val meth_createFromRawLong : Instrtree.t
val meth_createInfix : Instrtree.t
val meth_createInstance : Instrtree.t
val meth_createInt32 : Instrtree.t
val meth_createInt64 : Instrtree.t
val meth_createLong : Instrtree.t
val meth_createLongBlockFromSize : Instrtree.t
val meth_createNativeInt : Instrtree.t
val meth_createString : Instrtree.t
val meth_decrint : Instrtree.t
val meth_divint : Instrtree.t
val meth_divint32 : Instrtree.t
val meth_divint64 : Instrtree.t
val meth_equalValues : Instrtree.t
val meth_execute : Instrtree.t
val meth_executeWithBindings : Instrtree.t
val meth_fromStream : Instrtree.t
val meth_get : Instrtree.t
val meth_get'int : Instrtree.t
val meth_get_threadlocal : Instrtree.t
val meth_get0 : Instrtree.t
val meth_get1 : Instrtree.t
val meth_get2 : Instrtree.t
val meth_get3 : Instrtree.t
val meth_get4 : Instrtree.t
val meth_get5 : Instrtree.t
val meth_get6 : Instrtree.t
val meth_get7 : Instrtree.t
val meth_getAtom : Instrtree.t
val meth_getDouble : Instrtree.t
val meth_getDouble'int : Instrtree.t
val meth_getDouble0 : Instrtree.t
val meth_getDouble1 : Instrtree.t
val meth_getDouble2 : Instrtree.t
val meth_getDouble3 : Instrtree.t
val meth_getDouble4 : Instrtree.t
val meth_getDouble5 : Instrtree.t
val meth_getDouble6 : Instrtree.t
val meth_getDouble7 : Instrtree.t
val meth_getGenericDouble : Instrtree.t
val meth_getGenericDouble'int : Instrtree.t
val meth_getGenericDouble0 : Instrtree.t
val meth_getGenericDouble1 : Instrtree.t
val meth_getGenericDouble2 : Instrtree.t
val meth_getGenericDouble3 : Instrtree.t
val meth_getGenericDouble4 : Instrtree.t
val meth_getGenericDouble5 : Instrtree.t
val meth_getGenericDouble6 : Instrtree.t
val meth_getGenericDouble7 : Instrtree.t
val meth_getGlobal : Instrtree.t
val meth_getPredefinedExceptions : Instrtree.t
val meth_getRawLong : Instrtree.t
val meth_getRawValue : Instrtree.t
val meth_getResourceAsStream : Instrtree.t
val meth_getResult : Instrtree.t
val meth_getRunner : Instrtree.t
val meth_getUnsignedByte : Instrtree.t
val meth_globalStorage : Instrtree.t
val meth_greaterEqualValue : Instrtree.t
val meth_greaterThanValue : Instrtree.t
val meth_incrint : Instrtree.t
val meth_initGlobalBegin : Instrtree.t
val meth_initGlobalEnd : Instrtree.t
val meth_isBlock : Instrtree.t
val meth_isLong : Instrtree.t
val meth_isOut : Instrtree.t
val meth_isOutFirstPositive : Instrtree.t
val meth_isOutSecondPositive : Instrtree.t
val meth_loadConstants : Instrtree.t
val meth_lowerEqualValue : Instrtree.t
val meth_lowerThanValue : Instrtree.t
val meth_lslint : Instrtree.t
val meth_lsrint : Instrtree.t
val meth_modint : Instrtree.t
val meth_modint32 : Instrtree.t
val meth_modint64 : Instrtree.t
val meth_mulint : Instrtree.t
val meth_negint : Instrtree.t
val meth_notEqualValues : Instrtree.t
val meth_make_proxy_loader : Instrtree.t
val meth_make_proxy_system : Instrtree.t
val meth_make_proxy_runtime : Instrtree.t
val meth_offset : Instrtree.t
val meth_offsetint : Instrtree.t
val meth_offsetref : Instrtree.t
val meth_raise : Instrtree.t
val meth_send : Instrtree.t
val meth_send1 : Instrtree.t
val meth_send2 : Instrtree.t
val meth_send3 : Instrtree.t
val meth_send4 : Instrtree.t
val meth_send5 : Instrtree.t
val meth_send6 : Instrtree.t
val meth_send7 : Instrtree.t
val meth_send8 : Instrtree.t
val meth_set : Instrtree.t
val meth_set'int : Instrtree.t
val meth_set_threadlocal : Instrtree.t
val meth_set0 : Instrtree.t
val meth_set1 : Instrtree.t
val meth_set2 : Instrtree.t
val meth_set3 : Instrtree.t
val meth_set4 : Instrtree.t
val meth_set5 : Instrtree.t
val meth_set6 : Instrtree.t
val meth_set7 : Instrtree.t
val meth_setClosure1 : Instrtree.t
val meth_setClosureN : Instrtree.t
val meth_setConstant : Instrtree.t
val meth_setDouble : Instrtree.t
val meth_setDouble'int : Instrtree.t
val meth_setDouble0 : Instrtree.t
val meth_setDouble1 : Instrtree.t
val meth_setDouble2 : Instrtree.t
val meth_setDouble3 : Instrtree.t
val meth_setDouble4 : Instrtree.t
val meth_setDouble5 : Instrtree.t
val meth_setDouble6 : Instrtree.t
val meth_setDouble7 : Instrtree.t
val meth_setGenericDouble : Instrtree.t
val meth_setGenericDouble'int : Instrtree.t
val meth_setGenericDouble0 : Instrtree.t
val meth_setGenericDouble1 : Instrtree.t
val meth_setGenericDouble2 : Instrtree.t
val meth_setGenericDouble3 : Instrtree.t
val meth_setGenericDouble4 : Instrtree.t
val meth_setGenericDouble5 : Instrtree.t
val meth_setGenericDouble6 : Instrtree.t
val meth_setGenericDouble7 : Instrtree.t
val meth_setGlobal : Instrtree.t
val meth_setInfix1 : Instrtree.t
val meth_setInfixN : Instrtree.t
val meth_setRawLong : Instrtree.t
val meth_setRunner_awt : Instrtree.t
val meth_setRunner_graphics : Instrtree.t
val meth_setRunner_swing : Instrtree.t
val meth_setUnsignedByte : Instrtree.t
val meth_sizeBytes : Instrtree.t
val meth_subint : Instrtree.t
val meth_switchTag : Instrtree.t


(** {6 Peephole rewriting rules} *)

val peephole_rules : BaristaLibrary.Peephole.rewriting_rules list
(** The list of rewriting rules passed to Barista for peephole
    optimization. *)


(** {6 Utility functions} *)

val is_int32 : int -> bool
(** Tests whether the passed integer can be represented by a 32-bit long
    integer value. *)

val push_int32 : int32 -> Instrtree.t
(** Returns the instruction tree needed to push the passed integer. *)

val push_int64 : int64 -> Instrtree.t
(** Returns the instruction tree needed to push the passed integer. *)

val push_int : int -> Instrtree.t
(** Returns the instruction tree needed to push the passed integer. *)

val push_boxed_int : int -> Instrtree.t
(** Returns the instruction tree needed to push the passed integer as
    a value. *)

val push_double : float -> Instrtree.t
(** Returns the instruction tree needed to push the passed float. *)

val aload : int -> Instrtree.t
(** Returns the instruction tree needed to load a reference from a
    given index. *)

val astore : int -> Instrtree.t
(** Returns the instruction tree needed to store a reference into a
    given index. *)

val iload : int -> Instrtree.t
(** Returns the instruction tree needed to load an int from a given
    index. *)

val istore : int -> Instrtree.t
(** Returns the instruction tree needed to store an int into a given
    index. *)

val lload : int -> Instrtree.t
(** Returns the instruction tree needed to load a long from a given
    index. *)

val lstore : int -> Instrtree.t
(** Returns the instruction tree needed to store a long into a given
    index. *)

val dload : int -> Instrtree.t
(** Returns the instruction tree needed to load a double from a given
    index. *)

val dstore : int -> Instrtree.t
(** Returns the instruction tree needed to store a double into a given
    index. *)

val iinc : int -> int -> Instrtree.t
(** [iinx idx incr] returns the instruction tree needed to increment
    the int at index [idx] by [incr]. *)

val set_value : int -> Instrtree.t
(** Returns the instruction tree needed to store a value at a given
    index. *)
