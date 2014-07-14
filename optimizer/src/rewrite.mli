(*
 * This file is part of OCaml-Java optimizer.
 * Copyright (C) 2007-2014 Xavier Clerc.
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

(** Various sets of rewriting rules. *)


val remove_signals : BaristaLibrary.Peephole.rewriting_rules
(** Removes support for signals:
    - "invokestatic AbstractNativeRunner.checkSignals()" [->] "";
    - "invokestatic SignalSupport.*(...)" [->] "invokestatic NoSignalSupport.*(...)";
    - "getstatic SignalSupport.*" [->] "getstatic NoSignalSupport.*". *)

val remove_blocking_sections : BaristaLibrary.Peephole.rewriting_rules
(** Removes calls related to blocking sections:
    - "invokestatic CurrentContext.{enter,leave}BlockingSection()" [->] "". *)

val access_context_through_fields : BaristaLibrary.Peephole.rewriting_rules
(** Changes access to current context from methods to fields:
    - "invokestatic CurrentContext.getXyz()" [->] "getstatic CurrentContext.XYZ". *)

val safe_to_unsafe : BaristaLibrary.Peephole.rewriting_rules
(** Uses unsafe container classes instead of safe ones:
    - "new {BasicBlock,DoubleArray,LongBlock}Value" [->] "new Unsafe{BasicBlock,DoubleArray,LongBlock}Value";
    - "invokespecial {BasicBlock,DoubleArray,LongBlock}Value" [->] "invokespecial Unsafe{BasicBlock,DoubleArray,LongBlock}Value". *)

val constant_loading : BaristaLibrary.Peephole.rewriting_rules
(** Loads constants to "CONSTANTS" field:
    - "ldc[_w] string; invokestatic ThreadLocalFactory.globalStorage(String); putstatic cls.fld:ThreadLocal" [->] "";
    - "ldc[_w] cls; invokestatic ThreadLocalFactory.constantsStorage(cls):ThreadLocal"
    [->] "invokestatic cls.createConstants():void";
    - "putstatic cls.GLOBALS:ThreadLocal" [->] "putstatic cls.GLOBALS:Value";
    - "putstatic cls.CONSTANTS:ThreadLocal" [->] "". *)

val accesses_to_globals_and_constants : BaristaLibrary.Peephole.rewriting_rules
(** Changes accesses to {i CONSTANTS} and {i GLOBALS} fields, and stores
    the result of {i createGlobal} into the {i GLOBALS} field:
    - "getstatic cls.CONSTANTS:ThreadLocal; invokevirtual ThreadLocal.get():Object; checkcast cls2" [->] "";
    [->] "getstatic cls.GLOBALS:cls$Global";
    - "invokestatic cls.createGlobal(...)" [->] "putstatic curr_class.GLOBALS". *)

val remove_load_constant : BaristaLibrary.Peephole.rewriting_rules
(** Removes calls to {i AbstractNativeRunner.loadConstant(Class)}, and
    {i AbstractNativeRunner.setConstant(Class, Value)}:
    - "aload; ldc cls; invokestatic *.createConstants(); AbstractNativeRunner.setConstant(Class, Object)" [->] "". *)

val simplify_calls_to_entry : BaristaLibrary.Peephole.rewriting_rules
(** Removes storage of module entry result, and do not increment globals
    initialization counter (used for dynamic linking):
    - "aload_0; dup; invokestatic cls.entry(); putfield AbstractNativeRunner.result; invokevirtual AbstractNativeRunner.incrGlobalsInited()"
    [->] "invokestatic cls.entry(); pop". *)

val create_constants : BaristaLibrary.Peephole.rewriting_rules
(** Changes virtual accesses to constants into static accesses. *)

val remove_shared_constants : BaristaLibrary.Peephole.rewriting_rules
(** Removes instructions between
    "INVOKESTATIC AbstractNativeRunner.sharedConstantsBegin()" and
    "INVOKESTATIC AbstractNativeRunner.sharedConstantsEnd()" both
    inclusive. *)

val use_shared_constants : BaristaLibrary.Peephole.rewriting_rules
(** Rewrites "GETSTATIC cls.shared_constant" to
    "GETSTATIC shared_consts_class.shared_constant". *)

val remove_unused_globals : BaristaLibrary.Peephole.rewriting_rules
(** The compilation of an unused global has been replaced by a pushe of
    unit; this rule deletes "GETSTATIC Value.UNIT; pop" sequences. *)

val remove_nops : BaristaLibrary.Peephole.rewriting_rules
(** Removes "NOP" instructions. *)
