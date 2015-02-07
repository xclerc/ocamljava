(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
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
open Utils


(* Name functions *)

let normalize_function_name = function
  | "<init>" -> UTF8.of_string "<init>"
  | "<clinit>" -> UTF8.of_string "<clinit>"
  | s ->
      let buf = UTF8Buffer.make () in
      String.iter
        (fun c ->
          let c' = match c with
          | '.' -> UChar.of_code 0x00B7
          | ';' -> UChar.of_code 0x00A6
          | '[' -> UChar.of_code 0x00A1
          | '/' -> UChar.of_code 0x00F7
          | '<' -> UChar.of_code 0x00AB
          | '>' -> UChar.of_code 0x00BB
          | _ -> UChar.of_char c in
          UTF8Buffer.add_char buf c')
        s;
      UTF8Buffer.contents buf

let make_class s =
  s
  |> UTF8.of_string
  |> Name.make_for_class_from_external

let make_field s =
  s
  |> UTF8.of_string
  |> Name.make_for_field

let make_method s =
  s
  |> normalize_function_name
  |> Name.make_for_method


(* Class constants *)

let class_AbstractAwtApplet = JAVA_CLASS_NAME "AbstractAwtApplet"
let class_AbstractGraphicsApplet = JAVA_CLASS_NAME "AbstractGraphicsApplet"
let class_AbstractNativeRunner = JAVA_CLASS_NAME "AbstractNativeRunner"
let class_AbstractSwingApplet = JAVA_CLASS_NAME "AbstractSwingApplet"
let class_BlockValue = JAVA_CLASS_NAME "BlockValue"
let class_Class = JAVA_CLASS_NAME "Class"
let class_CompiledModule = JAVA_CLASS_NAME "CompiledModule"
let class_Constants = JAVA_CLASS_NAME "Constants"
let class_EntryPoint = JAVA_CLASS_NAME "EntryPoint"
let class_Fail = JAVA_CLASS_NAME "Fail"
let class_FailException = JAVA_CLASS_NAME "FailException"
let class_Global = JAVA_CLASS_NAME "Global"
let class_GlobalUses = JAVA_CLASS_NAME "GlobalUses"
let class_GlobalValue = JAVA_CLASS_NAME "GlobalValue"
let class_InputStream = JAVA_CLASS_NAME "InputStream"
let class_Int32Value = JAVA_CLASS_NAME "Int32Value"
let class_Int64Value = JAVA_CLASS_NAME "Int64Value"
let class_LongValue = JAVA_CLASS_NAME "LongValue"
let class_Map = JAVA_CLASS_NAME "Map"
let class_Math = JAVA_CLASS_NAME "Math"
let class_MethodHandle = JAVA_CLASS_NAME "MethodHandle"
let class_MethodMapping = JAVA_CLASS_NAME "org.ocamljava.runtime.primitives.javalibs.javabase.MethodMapping"
let class_NativeArithmetic = JAVA_CLASS_NAME "NativeArithmetic"
let class_NativeIntValue = JAVA_CLASS_NAME "NativeIntValue"
let class_Object = JAVA_CLASS_NAME "Object"
let class_Parameters = JAVA_CLASS_NAME "Parameters"
let class_PredefinedExceptions = JAVA_CLASS_NAME "org.ocamljava.runtime.context.PredefinedExceptions"
let class_Primitive = JAVA_CLASS_NAME "Primitive"
let class_PrimitiveProvider = JAVA_CLASS_NAME "PrimitiveProvider"
let class_PrintStream = JAVA_CLASS_NAME "PrintStream"
let class_String = JAVA_CLASS_NAME "String"
let class_System = JAVA_CLASS_NAME "System"
let class_ThreadLocal = JAVA_CLASS_NAME "ThreadLocal"
let class_Value = JAVA_CLASS_NAME "Value"
let type_Value = `Class class_Value

let interface_NativeParameters = JAVA_CLASS_NAME "NativeParameters"
let interface_OCamlJavaModule = JAVA_CLASS_NAME "OCamlJavaModule"


(* Field constants *)

let get_static c n t =
  Instrtree.leaf
    [ Instruction.GETSTATIC (c, make_field n, t) ]

let field_System_err = get_static class_System "err" (`Class class_PrintStream)
let field_System_in = get_static class_System "in" (`Class class_InputStream)
let field_System_out = get_static class_System "out" (`Class class_PrintStream)

let field_Value_EIGHT = JAVA_GET_STATIC "Value.EIGHT"
let field_Value_FALSE = JAVA_GET_STATIC "Value.FALSE"
let field_Value_FIVE = JAVA_GET_STATIC "Value.FIVE"
let field_Value_FOUR = JAVA_GET_STATIC "Value.FOUR"
let field_Value_MINUS_ONE = JAVA_GET_STATIC "Value.MINUS_ONE"
let field_Value_MINUS_TWO = JAVA_GET_STATIC "Value.MINUS_TWO"
let field_Value_ONE = JAVA_GET_STATIC "Value.ONE"
let field_Value_SEVEN = JAVA_GET_STATIC "Value.SEVEN"
let field_Value_SIX = JAVA_GET_STATIC "Value.SIX"
let field_Value_THREE = JAVA_GET_STATIC "Value.THREE"
let field_Value_TRUE = JAVA_GET_STATIC "Value.TRUE"
let field_Value_TWO = JAVA_GET_STATIC "Value.TWO"
let field_Value_UNIT = JAVA_GET_STATIC "Value.UNIT"
let field_Value_ZERO = JAVA_GET_STATIC "Value.ZERO"


(* Method constants *)

let meth_getResourceAsStream =
  Instrtree.leaf
    [ Instruction.INVOKEVIRTUAL ((`Class_or_interface class_Class),
                                 make_method "getResourceAsStream",
                                 ([`Class class_String], `Class class_InputStream)) ]
let meth_get_threadlocal =
  Instrtree.leaf
    [ Instruction.INVOKEVIRTUAL ((`Class_or_interface class_ThreadLocal),
                                 make_method "get",
                                 ([], `Class class_Object)) ]
let meth_set_threadlocal =
  Instrtree.leaf
    [ Instruction.INVOKEVIRTUAL ((`Class_or_interface class_ThreadLocal),
                                 make_method "set",
                                 ([`Class class_Object], `Void)) ]

let cstr_AbstractAwtApplet = JAVA_INVOKE_SPECIAL "AbstractAwtApplet()"
let cstr_AbstractGraphicsApplet = JAVA_INVOKE_SPECIAL "AbstractGraphicsApplet()"
let cstr_AbstractNativeRunner = JAVA_INVOKE_SPECIAL "AbstractNativeRunner(NativeParameters)"
let cstr_AbstractSwingApplet = JAVA_INVOKE_SPECIAL "AbstractSwingApplet()"
let cstr_GlobalValue = JAVA_INVOKE_SPECIAL "GlobalValue(int,long)"
let cstr_MethodMapping = JAVA_INVOKE_SPECIAL "org.ocamljava.runtime.primitives.javalibs.javabase.MethodMapping()"
let cstr_copy_AbstractNativeRunner = JAVA_INVOKE_SPECIAL "AbstractNativeRunner(AbstractNativeRunner)"

let meth_abs =
  Instrtree.leaf
    [ Instruction.INVOKESTATIC (class_Math, make_method "abs", ([`Double], `Double)) ]
let meth_add = JAVA_INVOKE_VIRTUAL "org.ocamljava.runtime.primitives.javalibs.javabase.MethodMapping.add(Class,String,Class[],String):void"
let meth_addint = JAVA_INVOKE_STATIC "NativeArithmetic.addint(long,long):long"
let meth_apply = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value[]):Value"
let meth_apply1 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value):Value"
let meth_apply2 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value):Value"
let meth_apply3 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value,Value):Value"
let meth_apply4 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value,Value,Value):Value"
let meth_apply5 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value,Value,Value,Value):Value"
let meth_apply6 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value,Value,Value,Value,Value):Value"
let meth_apply7 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value,Value,Value,Value,Value,Value):Value"
let meth_apply8 = JAVA_INVOKE_STATIC "NativeApply.apply(Value,Value,Value,Value,Value,Value,Value,Value,Value):Value"
let meth_arrayLength = JAVA_INVOKE_VIRTUAL "Value.arrayLength():long"
let meth_asCustom = JAVA_INVOKE_VIRTUAL "Value.asCustom():Object"
let meth_asDouble = JAVA_INVOKE_VIRTUAL "Value.asDouble():double"
let meth_asInt32 = JAVA_INVOKE_VIRTUAL "Value.asInt32():int"
let meth_asInt64 = JAVA_INVOKE_VIRTUAL "Value.asInt64():long"
let meth_asLong = JAVA_INVOKE_VIRTUAL "Value.asLong():long"
let meth_asNativeInt = JAVA_INVOKE_VIRTUAL "Value.asNativeInt():long"
let meth_asString = JAVA_INVOKE_VIRTUAL "Value.asString():String"
let meth_asValue = JAVA_INVOKE_STATIC "Fail.asValue(Throwable):Value"
let meth_asrint = JAVA_INVOKE_STATIC "NativeArithmetic.asrint(long,long):long"
let meth_bitvect_test = JAVA_INVOKE_STATIC "NativeUtils.caml_bitvect_test(Value,long):boolean"
let meth_caml_array_get = JAVA_INVOKE_STATIC "NativeUtils.caml_array_get(Value,long):Value"
let meth_caml_array_get_addr = JAVA_INVOKE_STATIC "NativeUtils.caml_array_get_addr(Value,long):Value"
let meth_caml_array_get_float = JAVA_INVOKE_STATIC "NativeUtils.caml_array_get_float(Value,long):double"
let meth_caml_array_get_int =  JAVA_INVOKE_STATIC "NativeUtils.caml_array_get_int(Value,long):long"
let meth_caml_array_set = JAVA_INVOKE_STATIC "NativeUtils.caml_array_set(Value,long,Value):Value"
let meth_caml_array_set_addr = JAVA_INVOKE_STATIC "NativeUtils.caml_array_set_addr(Value,long,Value):Value"
let meth_caml_array_set_float = JAVA_INVOKE_STATIC "NativeUtils.caml_array_set_float(Value,long,double):Value"
let meth_caml_array_set_int = JAVA_INVOKE_STATIC "NativeUtils.caml_array_set_int(Value,long,long):Value"
let meth_caml_array_unsafe_get = JAVA_INVOKE_STATIC "NativeUtils.caml_array_unsafe_get(Value,long):Value"
let meth_caml_array_unsafe_set = JAVA_INVOKE_STATIC "NativeUtils.caml_array_unsafe_set(Value,long,Value):Value"
let meth_caml_make_array = JAVA_INVOKE_STATIC "org.ocamljava.runtime.primitives.stdlib.Array.caml_make_array(Value):Value"
let meth_caml_string_get = JAVA_INVOKE_STATIC "NativeUtils.caml_string_get(Value,long):long"
let meth_caml_string_set = JAVA_INVOKE_STATIC "NativeUtils.caml_string_set(Value,long,long):Value"
let meth_checkSignals = JAVA_INVOKE_STATIC "AbstractNativeRunner.checkSignals():void"
let meth_constantsStorage = JAVA_INVOKE_STATIC "ThreadLocalFactory.constantsStorage(Class):ThreadLocal"
let meth_createBlock = JAVA_INVOKE_STATIC "Value.createBlock(int,long):Value"
let meth_createClosure = JAVA_INVOKE_STATIC "Value.createClosure(long):Value"
let meth_createClosure1 = JAVA_INVOKE_STATIC "Value.createClosure(MethodHandle):Value"
let meth_createClosureN = JAVA_INVOKE_STATIC "Value.createClosure(MethodHandle,int):Value"
let meth_createDouble = JAVA_INVOKE_STATIC "Value.createDouble(double):Value"
let meth_createDoubleArray = JAVA_INVOKE_STATIC "Value.createDoubleArray(long):Value"
let meth_createFromRawLong = JAVA_INVOKE_STATIC "Value.createFromRawLong(long):Value"
let meth_createInfix = JAVA_INVOKE_STATIC "Value.createInfix(int):Value"
let meth_createInstance = JAVA_INVOKE_STATIC "Value.createInstance(Object):Value"
let meth_createInt32 = JAVA_INVOKE_STATIC "Value.createInt32(int):Value"
let meth_createInt64 = JAVA_INVOKE_STATIC "Value.createInt64(long):Value"
let meth_createLong = JAVA_INVOKE_STATIC "Value.createLong(long):Value"
let meth_createLongBlockFromSize = JAVA_INVOKE_STATIC "Value.createLongBlockFromSize(int,long):Value"
let meth_createNativeInt = JAVA_INVOKE_STATIC "Value.createNativeInt(long):Value"
let meth_createString = JAVA_INVOKE_STATIC "Value.createString(String):Value"
let meth_decrint = JAVA_INVOKE_STATIC "NativeArithmetic.decrint(long):long"
let meth_divint = JAVA_INVOKE_STATIC "NativeArithmetic.divint(long,long):long"
let meth_divint32 = JAVA_INVOKE_STATIC "NativeArithmetic.divint32(int,int):int"
let meth_divint64 = JAVA_INVOKE_STATIC "NativeArithmetic.divint64(long,long):long"
let meth_equalValues = JAVA_INVOKE_STATIC "NativeComparisons.equalValues(Value,Value):boolean"
let meth_execute = JAVA_INVOKE_VIRTUAL "AbstractNativeRunner.execute():void"
let meth_executeWithBindings = JAVA_INVOKE_VIRTUAL "AbstractNativeRunner.executeWithBindings(Map):void"
let meth_fromStream = JAVA_INVOKE_STATIC "Parameters.fromStream(InputStream,String[],InputStream,PrintStream,PrintStream,boolean,String,Class):NativeParameters"
let meth_get = JAVA_INVOKE_VIRTUAL "Value.get(long):Value"
let meth_get'int = JAVA_INVOKE_VIRTUAL "Value.get(int):Value"
let meth_get0 = JAVA_INVOKE_VIRTUAL "Value.get0():Value"
let meth_get1 = JAVA_INVOKE_VIRTUAL "Value.get1():Value"
let meth_get2 = JAVA_INVOKE_VIRTUAL "Value.get2():Value"
let meth_get3 = JAVA_INVOKE_VIRTUAL "Value.get3():Value"
let meth_get4 = JAVA_INVOKE_VIRTUAL "Value.get4():Value"
let meth_get5 = JAVA_INVOKE_VIRTUAL "Value.get5():Value"
let meth_get6 = JAVA_INVOKE_VIRTUAL "Value.get6():Value"
let meth_get7 = JAVA_INVOKE_VIRTUAL "Value.get7():Value"
let meth_getAtom = JAVA_INVOKE_STATIC "AbstractNativeRunner.getAtom(int):Value"
let meth_getDouble = JAVA_INVOKE_VIRTUAL "Value.getDouble(long):double"
let meth_getDouble'int = JAVA_INVOKE_VIRTUAL "Value.getDouble(int):double"
let meth_getDouble0 = JAVA_INVOKE_VIRTUAL "Value.getDouble0():double"
let meth_getDouble1 = JAVA_INVOKE_VIRTUAL "Value.getDouble1():double"
let meth_getDouble2 = JAVA_INVOKE_VIRTUAL "Value.getDouble2():double"
let meth_getDouble3 = JAVA_INVOKE_VIRTUAL "Value.getDouble3():double"
let meth_getDouble4 = JAVA_INVOKE_VIRTUAL "Value.getDouble4():double"
let meth_getDouble5 = JAVA_INVOKE_VIRTUAL "Value.getDouble5():double"
let meth_getDouble6 = JAVA_INVOKE_VIRTUAL "Value.getDouble6():double"
let meth_getDouble7 = JAVA_INVOKE_VIRTUAL "Value.getDouble7():double"
let meth_getGenericDouble = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble(long):double"
let meth_getGenericDouble'int = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble(int):double"
let meth_getGenericDouble0 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble0():double"
let meth_getGenericDouble1 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble1():double"
let meth_getGenericDouble2 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble2():double"
let meth_getGenericDouble3 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble3():double"
let meth_getGenericDouble4 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble4():double"
let meth_getGenericDouble5 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble5():double"
let meth_getGenericDouble6 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble6():double"
let meth_getGenericDouble7 = JAVA_INVOKE_VIRTUAL "Value.getGenericDouble7():double"
let meth_getGlobal = JAVA_INVOKE_STATIC "AbstractNativeRunner.getGlobal(String):Value"
let meth_getPredefinedExceptions = JAVA_INVOKE_STATIC "org.ocamljava.runtime.context.CurrentContext.getPredefinedExceptions():org.ocamljava.runtime.context.PredefinedExceptions"
let meth_getRawLong = JAVA_INVOKE_VIRTUAL "Value.getRawLong(long):long"
let meth_getRawValue = JAVA_INVOKE_VIRTUAL "Value.getRawValue():long"
let meth_getResult = JAVA_INVOKE_VIRTUAL "AbstractNativeRunner.getResult():Value"
let meth_getRunner = JAVA_INVOKE_STATIC "AbstractNativeRunner.getRunner():AbstractNativeRunner"
let meth_getUnsignedByte = JAVA_INVOKE_VIRTUAL "Value.getUnsignedByte(long):int"
let meth_globalStorage = JAVA_INVOKE_STATIC "ThreadLocalFactory.globalStorage(String):ThreadLocal"
let meth_greaterEqualValue = JAVA_INVOKE_STATIC "NativeComparisons.greaterEqualValue(Value,Value):boolean"
let meth_greaterThanValue = JAVA_INVOKE_STATIC "NativeComparisons.greaterThanValue(Value,Value):boolean"
let meth_incrint = JAVA_INVOKE_STATIC "NativeArithmetic.incrint(long):long"
let meth_initGlobalBegin = JAVA_INVOKE_STATIC "AbstractNativeRunner.initGlobalBegin(int):void"
let meth_initGlobalEnd = JAVA_INVOKE_STATIC "AbstractNativeRunner.initGlobalEnd():void"
let meth_isBlock = JAVA_INVOKE_VIRTUAL "Value.isBlock():boolean"
let meth_isLong = JAVA_INVOKE_VIRTUAL "Value.isLong():boolean"
let meth_isOut = JAVA_INVOKE_STATIC "NativeComparisons.isOut(long,long):boolean"
let meth_isOutFirstPositive = JAVA_INVOKE_STATIC "NativeComparisons.isOutFirstPositive(long,long):boolean"
let meth_isOutSecondPositive = JAVA_INVOKE_STATIC "NativeComparisons.isOutFirstPositive(long,long):boolean"
let meth_loadConstants = JAVA_INVOKE_STATIC "AbstractNativeRunner.loadConstants(Class):Value"
let meth_lowerEqualValue = JAVA_INVOKE_STATIC "NativeComparisons.lowerEqualValue(Value,Value):boolean"
let meth_lowerThanValue = JAVA_INVOKE_STATIC "NativeComparisons.lowerThanValue(Value,Value):boolean"
let meth_lslint = JAVA_INVOKE_STATIC "NativeArithmetic.lslint(long,long):long"
let meth_lsrint = JAVA_INVOKE_STATIC "NativeArithmetic.lsrint(long,long):long"
let meth_modint = JAVA_INVOKE_STATIC "NativeArithmetic.modint(long,long):long"
let meth_modint32 = JAVA_INVOKE_STATIC "NativeArithmetic.modint32(int,int):int"
let meth_modint64 = JAVA_INVOKE_STATIC "NativeArithmetic.modint64(long,long):long"
let meth_mulint = JAVA_INVOKE_STATIC "NativeArithmetic.mulint(long,long):long"
let meth_negint = JAVA_INVOKE_STATIC "NativeArithmetic.negint(long):long"
let meth_notEqualValues = JAVA_INVOKE_STATIC "NativeComparisons.notEqualValues(Value,Value):boolean"
let meth_offset = JAVA_INVOKE_VIRTUAL "Value.offset(long):Value"
let meth_offsetint = JAVA_INVOKE_STATIC "NativeArithmetic.offsetint(long,long):long"
let meth_offsetref = JAVA_INVOKE_STATIC "NativeArithmetic.offsetref(Value,long):void"
let meth_make_proxy_loader = JAVA_INVOKE_STATIC "org.ocamljava.runtime.primitives.javalibs.javabase.Java.ocamljava_java_make_proxy_loader(Class[],ClassLoader,Value,org.ocamljava.runtime.primitives.javalibs.javabase.MethodMapping):Object"
let meth_make_proxy_system = JAVA_INVOKE_STATIC "org.ocamljava.runtime.primitives.javalibs.javabase.Java.ocamljava_java_make_proxy_system(Class[],Value,org.ocamljava.runtime.primitives.javalibs.javabase.MethodMapping):Object"
let meth_make_proxy_runtime = JAVA_INVOKE_STATIC "org.ocamljava.runtime.primitives.javalibs.javabase.Java.ocamljava_java_make_proxy_runtime(Class[],Value,org.ocamljava.runtime.primitives.javalibs.javabase.MethodMapping):Object"
let meth_raise = JAVA_INVOKE_STATIC "Fail.raise(Value):void"
let meth_send = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value[]):Value"
let meth_send1 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value):Value"
let meth_send2 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value):Value"
let meth_send3 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value,Value):Value"
let meth_send4 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value,Value,Value):Value"
let meth_send5 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value,Value,Value,Value):Value"
let meth_send6 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value,Value,Value,Value,Value):Value"
let meth_send7 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value,Value,Value,Value,Value,Value):Value"
let meth_send8 = JAVA_INVOKE_STATIC "NativeApply.send(Value,Value,Value,Value,Value,Value,Value,Value,Value,Value,Value):Value"
let meth_set = JAVA_INVOKE_VIRTUAL "Value.set(long,Value):void"
let meth_set'int = JAVA_INVOKE_VIRTUAL "Value.set(int,Value):void"
let meth_set0 = JAVA_INVOKE_VIRTUAL "Value.set0(Value):void"
let meth_set1 = JAVA_INVOKE_VIRTUAL "Value.set1(Value):void"
let meth_set2 = JAVA_INVOKE_VIRTUAL "Value.set2(Value):void"
let meth_set3 = JAVA_INVOKE_VIRTUAL "Value.set3(Value):void"
let meth_set4 = JAVA_INVOKE_VIRTUAL "Value.set4(Value):void"
let meth_set5 = JAVA_INVOKE_VIRTUAL "Value.set5(Value):void"
let meth_set6 = JAVA_INVOKE_VIRTUAL "Value.set6(Value):void"
let meth_set7 = JAVA_INVOKE_VIRTUAL "Value.set7(Value):void"
let meth_setClosure1 = JAVA_INVOKE_VIRTUAL "Value.setClosure(MethodHandle):Value"
let meth_setClosureN = JAVA_INVOKE_VIRTUAL "Value.setClosure(MethodHandle,int):Value"
let meth_setConstant = JAVA_INVOKE_VIRTUAL "AbstractNativeRunner.setConstant(Class,Object):void"
let meth_setDouble = JAVA_INVOKE_VIRTUAL "Value.setDouble(long,double):void"
let meth_setDouble'int = JAVA_INVOKE_VIRTUAL "Value.setDouble(int,double):void"
let meth_setDouble0 = JAVA_INVOKE_VIRTUAL "Value.setDouble0(double):void"
let meth_setDouble1 = JAVA_INVOKE_VIRTUAL "Value.setDouble1(double):void"
let meth_setDouble2 = JAVA_INVOKE_VIRTUAL "Value.setDouble2(double):void"
let meth_setDouble3 = JAVA_INVOKE_VIRTUAL "Value.setDouble3(double):void"
let meth_setDouble4 = JAVA_INVOKE_VIRTUAL "Value.setDouble4(double):void"
let meth_setDouble5 = JAVA_INVOKE_VIRTUAL "Value.setDouble5(double):void"
let meth_setDouble6 = JAVA_INVOKE_VIRTUAL "Value.setDouble6(double):void"
let meth_setDouble7 = JAVA_INVOKE_VIRTUAL "Value.setDouble7(double):void"
let meth_setGenericDouble = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble(long,double):void"
let meth_setGenericDouble'int = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble(int,double):void"
let meth_setGenericDouble0 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble0(double):void"
let meth_setGenericDouble1 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble1(double):void"
let meth_setGenericDouble2 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble2(double):void"
let meth_setGenericDouble3 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble3(double):void"
let meth_setGenericDouble4 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble4(double):void"
let meth_setGenericDouble5 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble5(double):void"
let meth_setGenericDouble6 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble6(double):void"
let meth_setGenericDouble7 = JAVA_INVOKE_VIRTUAL "Value.setGenericDouble7(double):void"
let meth_setGlobal = JAVA_INVOKE_STATIC "AbstractNativeRunner.setGlobal(String,Value):void"
let meth_setInfix1 = JAVA_INVOKE_VIRTUAL "Value.setInfix(MethodHandle,long):Value"
let meth_setInfixN = JAVA_INVOKE_VIRTUAL "Value.setInfix(MethodHandle,long,int):Value"
let meth_setRawLong = JAVA_INVOKE_VIRTUAL "Value.setRawLong(long,long):void"
let meth_setRunner_awt =  JAVA_INVOKE_VIRTUAL "AbstractAwtApplet.setRunner(AbstractNativeRunner):void"
let meth_setRunner_graphics =  JAVA_INVOKE_VIRTUAL "AbstractGraphicsApplet.setRunner(AbstractNativeRunner):void"
let meth_setRunner_swing =  JAVA_INVOKE_VIRTUAL "AbstractSwingApplet.setRunner(AbstractNativeRunner):void"
let meth_setUnsignedByte = JAVA_INVOKE_VIRTUAL "Value.setUnsignedByte(long,int):void"
let meth_sizeBytes = JAVA_INVOKE_VIRTUAL "Value.sizeBytes():long"
let meth_subint = JAVA_INVOKE_STATIC "NativeArithmetic.subint(long,long):long"
let meth_switchTag = JAVA_INVOKE_VIRTUAL "Value.switchTag():int"


(* Peephole rewriting rules *)

let rewrite_constants l =
  let get_static c n t = Instruction.GETSTATIC (c, make_field n, t) in
  let field_Value_ZERO = get_static class_Value "ZERO" type_Value in
  let field_Value_ONE = get_static class_Value "ONE" type_Value in
  let field_Value_TWO = get_static class_Value "TWO" type_Value in
  let field_Value_THREE = get_static class_Value "THREE" type_Value in
  let field_Value_FOUR = get_static class_Value "FOUR" type_Value in
  let field_Value_FIVE = get_static class_Value "FIVE" type_Value in
  let field_Value_SIX = get_static class_Value "SIX" type_Value in
  let field_Value_SEVEN = get_static class_Value "SEVEN" type_Value in
  let field_Value_EIGHT = get_static class_Value "EIGHT" type_Value in
  let field_Value_MINUS_ONE = get_static class_Value "MINUS_ONE" type_Value in
  let field_Value_MINUS_TWO = get_static class_Value "MINUS_TWO" type_Value in
  (* assumes that Peephole.optimize_constants has been applied before *)
  let rec rewrite acc = function
    | (line, Instruction.LCONST_0) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_ZERO) :: acc) tl
    | (line, Instruction.LCONST_1) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_ONE) :: acc) tl
    | (line, Instruction.ICONST_2) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_TWO) :: acc) tl
    | (line, Instruction.ICONST_3) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_THREE) :: acc) tl
    | (line, Instruction.ICONST_4) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_FOUR) :: acc) tl
    | (line, Instruction.ICONST_5) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_FIVE) :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when ((cst :> int) = 6) && (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_SIX) :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when ((cst :> int) = 7) && (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_SEVEN) :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when ((cst :> int) = 8) && (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_EIGHT) :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when ((cst :> int) = -1) && (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_MINUS_ONE) :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, Instruction.INVOKESTATIC (cn, mn, _)) :: tl
      when ((cst :> int) = -2) && (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createLong") ->
            rewrite ((line, field_Value_MINUS_TWO) :: acc) tl
    | (line, Instruction.GETSTATIC (cn, fn, _)) :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn, _)) :: (_, Instruction.L2I) :: tl
      when (Name.equal_for_class class_Value cn) && (Name.equal_for_class class_Value cn')
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "asLong")
          && ((UTF8.to_string (Name.utf8_for_field fn)) = "ZERO") ->
            rewrite ((line, Instruction.ICONST_0) :: acc) tl
    | (line, Instruction.GETSTATIC (cn, fn, _)) :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn, _)) :: (_, Instruction.L2I) :: tl
      when (Name.equal_for_class class_Value cn) && (Name.equal_for_class class_Value cn')
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "asLong")
          && ((UTF8.to_string (Name.utf8_for_field fn)) = "ONE") ->
            rewrite ((line, Instruction.ICONST_1) :: acc) tl
    | (line, Instruction.GETSTATIC (cn, fn, _)) :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn, _)) :: tl
      when (Name.equal_for_class class_Value cn) && (Name.equal_for_class class_Value cn')
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "asLong")
          && ((UTF8.to_string (Name.utf8_for_field fn)) = "ZERO") ->
            rewrite ((line, Instruction.LCONST_0) :: acc) tl
    | (line, Instruction.GETSTATIC (cn, fn, _)) :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn, _)) :: tl
      when (Name.equal_for_class class_Value cn) && (Name.equal_for_class class_Value cn')
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "asLong")
          && ((UTF8.to_string (Name.utf8_for_field fn)) = "ONE") ->
            rewrite ((line, Instruction.LCONST_1) :: acc) tl
    | (line, Instruction.GETSTATIC (cn, fn, _)) :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn, _)) :: tl
      when (Name.equal_for_class class_Value cn) && (Name.equal_for_class class_Value cn')
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "getRawValue")
          && ((UTF8.to_string (Name.utf8_for_field fn)) = "ZERO") ->
            rewrite ((line, Instruction.LCONST_1) :: acc) tl
    | (line, Instruction.GETSTATIC (cn, fn, _)) :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn, _)) :: tl
      when (Name.equal_for_class class_Value cn) && (Name.equal_for_class class_Value cn')
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "getRawValue")
          && ((UTF8.to_string (Name.utf8_for_field fn)) = "UNIT") ->
            rewrite ((line, Instruction.LCONST_1) :: acc) tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let rewrite_optimized_get l =
  (* assumes that Peephole.optimize_constants has been applied before *)
  let is_get_call i =
    match i with
    | Instruction.INVOKEVIRTUAL ((`Class_or_interface cn), mn, _) ->
        (Name.equal_for_class class_Value cn)
          && (List.mem (UTF8.to_string (Name.utf8_for_method mn))
                [ "get"; "getDouble"; "getGenericDouble"; "getRawLong" ])
    | _ -> false in
  let change_call i idx =
    match i with
    | Instruction.INVOKEVIRTUAL (cn, mn, (params, ret)) ->
        let mn = UTF8.to_string (Name.utf8_for_method mn) in
        let mn = mn ^ idx in
        let mn = Name.make_for_method (UTF8.of_string mn) in
        let params = List.tl params in
        Instruction.INVOKEVIRTUAL (cn, mn, (params, ret))
    | _ -> assert false in
  let rec rewrite acc = function
    | (line, Instruction.LCONST_0) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "0") :: acc) tl
    | (line, Instruction.LCONST_1) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "1") :: acc) tl
    | (line, Instruction.ICONST_2) :: (_, Instruction.I2L) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "2") :: acc) tl
    | (line, Instruction.ICONST_3) :: (_, Instruction.I2L) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "3") :: acc) tl
    | (line, Instruction.ICONST_4) :: (_, Instruction.I2L) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "4") :: acc) tl
    | (line, Instruction.ICONST_5) :: (_, Instruction.I2L) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "5") :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, instr) :: tl
      when ((cst :> int) = 6) && (is_get_call instr) ->
        rewrite ((line, change_call instr "6") :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, Instruction.I2L) :: (_, instr) :: tl
      when ((cst :> int) = 7) && (is_get_call instr) ->
        rewrite ((line, change_call instr "7") :: acc) tl
    | (line, Instruction.ICONST_0) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "0") :: acc) tl
    | (line, Instruction.ICONST_1) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "1") :: acc) tl
    | (line, Instruction.ICONST_2) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "2") :: acc) tl
    | (line, Instruction.ICONST_3) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "3") :: acc) tl
    | (line, Instruction.ICONST_4) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "4") :: acc) tl
    | (line, Instruction.ICONST_5) :: (_, instr) :: tl
      when is_get_call instr ->
        rewrite ((line, change_call instr "5") :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, instr) :: tl
      when ((cst :> int) = 6) && (is_get_call instr) ->
        rewrite ((line, change_call instr "6") :: acc) tl
    | (line, Instruction.BIPUSH cst) :: (_, instr) :: tl
      when ((cst :> int) = 7) && (is_get_call instr) ->
        rewrite ((line, change_call instr "7") :: acc) tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let rewrite_unbox_box l =
  let symmetric x y =
    match x, y with
    | "getRawValue", "createFromRawValue"
    | "getRawValue", "createFromRawLong"
    | "asLong", "createLong"
    | "asCustom", "createInstance"
    | "asDouble", "createDouble"
    | "asInt32", "createInt32"
    | "asInt64", "createInt64"
    | "asNativeInt", "createNativeInt" ->
        true
    | _ -> false in
  let rec rewrite acc = function
    | (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn, mn, _))
        :: (_, Instruction.INVOKESTATIC (cn', mn', _))
        :: tl
      when (Name.equal_for_class class_Value cn)
          && (Name.equal_for_class class_Value cn')
          && symmetric
          (UTF8.to_string (Name.utf8_for_method mn))
          (UTF8.to_string (Name.utf8_for_method mn')) ->
            rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let rewrite_box_unbox l =
  let symmetric x y =
    match x, y with
    | "createFromRawValue", "getRawValue"
    | "createFromRawLong", "getRawValue"
    | "createLong", "asLong"
    | "createInstance", "asCustom"
    | "createDouble", "asDouble"
    | "createInt32", "asInt32"
    | "createInt64", "asInt64"
    | "createNativeInt", "asNativeInt" ->
        true
    | _ -> false in
  let rec rewrite acc = function
    | (_, Instruction.INVOKESTATIC (cn, mn, _))
        :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn', _))
        :: tl
      when (Name.equal_for_class class_Value cn)
          && (Name.equal_for_class class_Value cn')
          && symmetric
          (UTF8.to_string (Name.utf8_for_method mn))
          (UTF8.to_string (Name.utf8_for_method mn')) ->
            rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let rewrite_gets l =
  let aload_index = function
    | Instruction.ALOAD_0 -> Some 0
    | Instruction.ALOAD_1 -> Some 1
    | Instruction.ALOAD_2 -> Some 2
    | Instruction.ALOAD_3 -> Some 3
    | Instruction.ALOAD n -> Some (n :> int)
    | Instruction.WIDE_ALOAD n -> Some (n :> int)
    | _ -> None in
  let same_aload instr1 instr2 =
    match aload_index instr1, aload_index instr2 with
    | Some x, Some y -> x = y
    | _ -> false in
  let is_pure_get = function
    | "get0" | "get1" | "get2" | "get3" | "get4" | "get5" | "get6" | "get7"
    | "getDouble0" | "getDouble1" | "getDouble2" | "getDouble3" | "getDouble4" | "getDouble5" | "getDouble6" | "getDouble7"
    | "getGenericDouble0" | "getGenericDouble1" | "getGenericDouble2" | "getGenericDouble3" | "getGenericDouble4" | "getGenericDouble5" | "getGenericDouble6" | "getGenericDouble7"
    | "getRawLong0" | "getRawLong1" | "getRawLong2" | "getRawLong3" | "getRawLong4" | "getRawLong5" | "getRawLong6" | "getRawLong7"
    | "asLong"
    | "asDouble"
    | "getRawValue"
    | "asBlock"
    | "asInt32"
    | "asInt64"
    | "asNativeInt" ->
        true
    | _ ->
        false in
  let rec rewrite acc = function
    | ((_, aload) as first)
      :: ((line, Instruction.INVOKEVIRTUAL (`Class_or_interface cn, mn, md)) as second)
      :: (_, aload')
      :: (_, Instruction.INVOKEVIRTUAL (`Class_or_interface cn', mn', md'))
      :: tl
      when (same_aload aload aload')
          && (Name.equal_for_class cn cn')
          && (Name.equal_for_method mn mn')
          && (Descriptor.equal_for_method md md')
          && (is_pure_get (UTF8.to_string (Name.utf8_for_method mn))) ->
            let dup = match snd md with
            | `Long | `Double -> Instruction.DUP2
            | _ -> Instruction.DUP in
            let acc = (line, dup) :: second :: first :: acc in
            rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let rewrite_null l =
  let rec rewrite acc = function
    | (line, Instruction.ACONST_NULL)
      :: (_, Instruction.INVOKESTATIC (cn, mn, _))
      :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "createInstance") ->
            let get = Instruction.GETSTATIC (class_Value, make_field "NULL", type_Value) in
            let acc = (line, get) :: acc in
            rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let remove_useless_casts l =
  let rec rewrite acc = function
    | ((line, Instruction.INVOKEVIRTUAL (`Class_or_interface cn, mn, _)) as call)
      :: (_, Instruction.CHECKCAST (`Class_or_interface cn'))
      :: tl
      when (Name.equal_for_class class_Value cn)
          && ((UTF8.to_string (Name.utf8_for_method mn)) = "asCustom")
          && (Name.equal_for_class class_Object cn') ->
            let acc = call :: acc in
            rewrite acc tl
    | hd :: tl -> rewrite (hd :: acc) tl
    | [] -> List.rev acc in
  rewrite [] l

let peephole_rules =
  Peephole.all_rules
  @ [ rewrite_constants ;
      rewrite_optimized_get ;
      rewrite_unbox_box ;
      rewrite_box_unbox ;
      rewrite_gets ;
      rewrite_null ;
      remove_useless_casts ]


(* Utility functions *)

let fatal_error s =
  Misc.fatal_error ("Bytecodeutils." ^ s)

let is_int32 x =
  x >= (-2147483648) && x <= 2147483647

let push_int32 x =
  let res = match x with
  | (-1l) -> Instruction.ICONST_M1
  | 0l -> Instruction.ICONST_0
  | 1l -> Instruction.ICONST_1
  | 2l -> Instruction.ICONST_2
  | 3l -> Instruction.ICONST_3
  | 4l -> Instruction.ICONST_4
  | 5l -> Instruction.ICONST_5
  | n when n >= (-128l) && n <= 127l -> Instruction.BIPUSH (s1 (Int32.to_int n))
  | n when n >= (-32768l) && n <= 32767l -> Instruction.SIPUSH (s2 (Int32.to_int n))
  | n -> Instruction.LDC_W (`Int n) in
  Instrtree.leaf [ res ]

let push_int64 x =
  let res = match x with
  | 0L -> Instruction.LCONST_0
  | 1L -> Instruction.LCONST_1
  | n -> Instruction.LDC2_W (`Long n) in
  Instrtree.leaf [ res ]

let push_int x =
  let res = match x with
  | (-1) -> [ Instruction.ICONST_M1; Instruction.I2L ]
  | 0 -> [ Instruction.LCONST_0 ]
  | 1 -> [ Instruction.LCONST_1 ]
  | 2 -> [ Instruction.ICONST_2; Instruction.I2L ]
  | 3 -> [ Instruction.ICONST_3; Instruction.I2L ]
  | 4 -> [ Instruction.ICONST_4; Instruction.I2L ]
  | 5 -> [ Instruction.ICONST_5; Instruction.I2L ]
  | n when n >= (-128) && n <= 127 ->
      [ Instruction.BIPUSH (s1 n);
        Instruction.I2L ]
  | n when n >= (-32768) && n <= 32767 ->
      [ Instruction.SIPUSH (s2 n);
        Instruction.I2L ]
  | n when n >= (-2147483648) && n <= 2147483647 ->
      [ Instruction.LDC_W (`Int (Int32.of_int n));
        Instruction.I2L ]
  | n ->
      [ Instruction.LDC2_W (`Long (Int64.of_int n)) ] in
  Instrtree.leaf res

let push_boxed_int = function
  | -2 -> field_Value_MINUS_TWO
  | -1 -> field_Value_MINUS_ONE
  | 0 -> field_Value_ZERO
  | 1 -> field_Value_ONE
  | 2 -> field_Value_TWO
  | 3 -> field_Value_THREE
  | 4 -> field_Value_FOUR
  | 5 -> field_Value_FIVE
  | 6 -> field_Value_SIX
  | 7 -> field_Value_SEVEN
  | 8 -> field_Value_EIGHT
  | n -> Instrtree.node [ push_int n ; meth_createLong ]

let push_double x =
  let res = match x with
  | 0. -> Instruction.DCONST_0
  | 1. -> Instruction.DCONST_1
  | d -> Instruction.LDC2_W (`Double d) in
  Instrtree.leaf [ res ]

let aload idx =
  let res = match idx with
  | 0 -> Instruction.ALOAD_0
  | 1 -> Instruction.ALOAD_1
  | 2 -> Instruction.ALOAD_2
  | 3 -> Instruction.ALOAD_3
  | n when n >= 0 && n <= 255 -> Instruction.ALOAD (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_ALOAD (u2 n)
  | _ -> fatal_error "aload: invalid index" in
  Instrtree.leaf [ res ]

let astore idx =
  let res = match idx with
  | 0 -> Instruction.ASTORE_0
  | 1 -> Instruction.ASTORE_1
  | 2 -> Instruction.ASTORE_2
  | 3 -> Instruction.ASTORE_3
  | n when n >= 0 && n <= 255 -> Instruction.ASTORE (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_ASTORE (u2 n)
  | _ -> fatal_error "astore: invalid index" in
  Instrtree.leaf [ res ]

let iload idx =
  let res = match idx with
  | 0 -> Instruction.ILOAD_0
  | 1 -> Instruction.ILOAD_1
  | 2 -> Instruction.ILOAD_2
  | 3 -> Instruction.ILOAD_3
  | n when n >= 0 && n <= 255 -> Instruction.ILOAD (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_ILOAD (u2 n)
  | _ -> fatal_error "iload: invalid index" in
  Instrtree.leaf [ res ]

let istore idx =
  let res = match idx with
  | 0 -> Instruction.ISTORE_0
  | 1 -> Instruction.ISTORE_1
  | 2 -> Instruction.ISTORE_2
  | 3 -> Instruction.ISTORE_3
  | n when n >= 0 && n <= 255 -> Instruction.ISTORE (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_ISTORE (u2 n)
  | _ -> fatal_error "istore: invalid index" in
  Instrtree.leaf [ res ]

let lload idx =
  let res = match idx with
  | 0 -> Instruction.LLOAD_0
  | 1 -> Instruction.LLOAD_1
  | 2 -> Instruction.LLOAD_2
  | 3 -> Instruction.LLOAD_3
  | n when n >= 0 && n <= 255 -> Instruction.LLOAD (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_LLOAD (u2 n)
  | _ -> fatal_error "lload: invalid index" in
  Instrtree.leaf [ res ]

let lstore idx =
  let res = match idx with
  | 0 -> Instruction.LSTORE_0
  | 1 -> Instruction.LSTORE_1
  | 2 -> Instruction.LSTORE_2
  | 3 -> Instruction.LSTORE_3
  | n when n >= 0 && n <= 255 -> Instruction.LSTORE (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_LSTORE (u2 n)
  | _ -> fatal_error "lstore: invalid index" in
  Instrtree.leaf [ res ]

let dload idx =
  let res = match idx with
  | 0 -> Instruction.DLOAD_0
  | 1 -> Instruction.DLOAD_1
  | 2 -> Instruction.DLOAD_2
  | 3 -> Instruction.DLOAD_3
  | n when n >= 0 && n <= 255 -> Instruction.DLOAD (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_DLOAD (u2 n)
  | _ -> fatal_error "dload: invalid index" in
  Instrtree.leaf [ res ]

let dstore idx =
  let res = match idx with
  | 0 -> Instruction.DSTORE_0
  | 1 -> Instruction.DSTORE_1
  | 2 -> Instruction.DSTORE_2
  | 3 -> Instruction.DSTORE_3
  | n when n >= 0 && n <= 255 -> Instruction.DSTORE (u1 n)
  | n when n >= 0 && n <= 65535 -> Instruction.WIDE_DSTORE (u2 n)
  | _ -> fatal_error "dstore: invalid index" in
  Instrtree.leaf [ res ]

let iinc idx incr =
  let res = match idx, incr with
  | n, n' when n >= 0 && n <= 255 && n' >= -128 && n' <= 127 ->
      Instruction.IINC (u1 n, s1 n')
  | n, n' when n >= 0 && n <= 65535 && n' >= -32768 && n' <= 32767 ->
      Instruction.WIDE_IINC (u2 n, s2 n')
  | _ -> fatal_error "iinc: invalid index or increment" in
  Instrtree.leaf [ res ]

let set_value idx =
  let set idx =
    let mth = Printf.sprintf "set%d" idx in
    Instrtree.leaf [ Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                                make_method mth,
                                                ([`Class class_Value], `Void)) ] in
  match idx with
  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 ->
      set idx
  | _ ->
      Instrtree.node [ push_int idx ;
                       Instrtree.leaf [ Instruction.DUP2_X1 ;
                                        Instruction.POP2 ;
                                        Instruction.INVOKEVIRTUAL (`Class_or_interface class_Value,
                                                                   make_method "set",
                                                                   ([`Long; `Class class_Value], `Void)) ] ]
