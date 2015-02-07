(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java optimizer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java optimizer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* class names *)

let abstract_native_runner = Misc.make_class_name_ext "org.ocamljava.runtime.kernel.AbstractNativeRunner"

let class_ = Misc.make_class_name_ext "java.lang.Class"

let constants_class = Misc.make_class_name_ext "org.ocamljava.runtime.annotations.markers.Constants"

let current_context = Misc.make_class_name_ext "org.ocamljava.runtime.context.CurrentContext"

let debug = Misc.make_class_name_ext "org.ocamljava.runtime.kernel.Debug"

let entry_point = Misc.make_class_name_ext "org.ocamljava.runtime.annotations.markers.EntryPoint"

let fail_exception = Misc.make_class_name_ext "org.ocamljava.runtime.kernel.FailException"

let global_uses = Misc.make_class_name_ext "org.ocamljava.runtime.annotations.markers.GlobalUses"

let no_signal_support = Misc.make_class_name_ext "org.ocamljava.runtime.util.NoSignalSupport"

let object_ = Misc.make_class_name_ext "java.lang.Object"

let ocamljava_module = Misc.make_class_name_ext "org.ocamljava.runtime.kernel.OCamlJavaModule"

let signal_support = Misc.make_class_name_ext "org.ocamljava.runtime.util.SignalSupport"

let string = Misc.make_class_name_ext "java.lang.String"

let thread_local = Misc.make_class_name_ext "java.lang.ThreadLocal"

let thread_local_factory = Misc.make_class_name_ext "org.ocamljava.runtime.kernel.ThreadLocalFactory"

let value = Misc.make_class_name_ext "org.ocamljava.runtime.values.Value"


(* method names *)

let begin_ = Misc.make_method_name "begin"

let check_signals = Misc.make_method_name "checkSignals"

let constants_storage = Misc.make_method_name "constantsStorage"

let create_constants = Misc.make_method_name "createConstants"

let cstr_name = Misc.make_method_name "<init>"

let end_ = Misc.make_method_name "end"

let enter_blocking_section = Misc.make_method_name "enterBlockingSection"

let entry = Misc.make_method_name "entry"

let fill_in_stack_trace = Misc.make_method_name "fillInStackTrace"

let get = Misc.make_method_name "get"

let global_storage = Misc.make_method_name "globalStorage"

let incr_globals_inited = Misc.make_method_name "incrGlobalsInited"

let init_global_begin = Misc.make_method_name "initGlobalBegin"

let init_global_end = Misc.make_method_name "initGlobalEnd"

let leave_blocking_section = Misc.make_method_name "leaveBlockingSection"

let main_scripting = Misc.make_method_name "mainScripting"

let main_with_return = Misc.make_method_name "mainWithReturn"

let module_main = Misc.make_method_name "moduleMain"

let set_constant = Misc.make_method_name "setConstant"

let set_global = Misc.make_method_name "setGlobal"

let shared_constants_begin = Misc.make_method_name "sharedConstantsBegin"

let shared_constants_end = Misc.make_method_name "sharedConstantsEnd"


(* field names *)

let constants = Misc.make_field_name "CONSTANTS"

let globals = Misc.make_field_name "GLOBALS"

let result = Misc.make_field_name "result"

let unit = Misc.make_field_name "UNIT"


(* attribute names *)

let linked_classes = BaristaLibrary.UTF8.of_string "linkedClasses"

let standalone = BaristaLibrary.UTF8.of_string "standalone"
