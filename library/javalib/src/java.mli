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

(** Manipulation of Java references.

    The [ocamljava] compiler allows to manipulate Java references through
    two types, namely [cn java_instance] and [cn java_extends]. The
    former is used to designate instances of exactly one class, while
    the latter is used to designate instances of either one class or
    any of its subclasses. In both cases, the type parameter is used to
    specify the class. It uses the fully-qualified name of the class,
    just replacing dots with single quotes. The type of Java strings is
    thus written [java'lang'String java_instance].

    This module contains the functions needed to create new instances,
    call methods, and access fields. Such functions use {i descriptors}
    to determine the constructor/method/field to use. These descriptors
    are written as string litterals; for example, parsing a string into
    an integer can be done by writing:
    {[
      let integer = call "java.lang.Integer.parseInt(java.lang.String):int" s
    ]}

    However, it is possible to use simple names rather than qualified
    names for classes if their packages have been opened. Initially, the
    {i java.lang} package is the only one opened. A package can be opened
    though a modified [open Package'packname] directive; for example
    opening the {i javax.awt} package can be done by writing:
    {[
      open Package'java'awt
    ]}
    It is also possible to take advantage of opened packages in types, by
    replacing the package name with an underscore. The type of Java
    strings can thus be written [_'String java_instance].

    Moreover, types of fields and return types of methods can be elided
    as long as there is no ambiguity. Types of method/constructor
    parameters can also be replaced with single underscores, leading to
    the lighter code to parse an integer:
    {[
      let integer = call "Integer.parseInt(_)" s
    ]}
    Furthermore, it is possible to use dashes to denote any number of
    parameters, leading to the lightest code to rotate an integer value:
    {[
      let y = call "Integer.rotateLeft(-)" x bits
    ]}
    The compiler will issue an error if there is an ambiguity.

    When calling a method with a variable number of arguments, two
    variants are accepted for the descriptor, impacting how the arguments
    should be passed:
    - if ["C.m(T[])"] is used, then the arguments are passed through a
      Java array;
    - if ["C.m(T...)"] is used, then the arguments are passed through an
      OCaml literal array.
    The following code illustrates the alternative:
    {[
      let a1 = Java.make_array "Object[]" 5l
      let l1 = Java.call "Arrays.asList(Object[])" a1
      let l2 =
        Java.call "Arrays.asList(Object...)"
          [| Java.null ; Java.make "Object()" () |]
    ]}

    Finally, two exceptions are defined to wrap Java exceptions on the
    OCaml side:
    {[
      exception Java_exception of java'lang'Exception java_instance
      exception Java_error of java'lang'Error java_instance
    ]}

    {b Warning:} to be able to use the functions from this module, java
    extensions should be enabled by passing the [-java-extensions] option
    to the [ocamljava] compiler. *)


(** {6 Instance creation} *)

external make : 'a java_constructor -> 'a =
  "java constructor"
(** [make "constructordesc" p0 ... pn] calls constructor
    [constructordesc] with parameters [pi] and returns the created
    instance. For example, the following code creates an object instance:
    {[
      let inst = make "java.lang.Object()" ()
    ]}

    Raises [Java_exception] if the constructor throws an exception. *)

external make_array : 'a java_array_shape -> 'a =
  "java make array"
(** [make_array "arraydesc" dim1 ... dimn] builds and returns an array
    with [n] dimensions. Each element is initialized to the default value
    (that is false for booleans, and zero for other primitive types), and
    {i null} for reference types).

    For example, the following code creates a 2x3 matrix of byte values:
    {[
      let arr = make_array "byte[][]" 2l 3l
    ]}

    Raises [Java_exception] if a dimension is negative. *)

external make_array_dims : 'a java_array_shape_dims -> 'a =
  "java make array dims"
(** [make_array_dims "arraydesc" dim1 ... dimn] is similar to
    [make_array], except that the array descriptor is made of two kinds
    of dimension specifiers:
    - {i \[_\]} that indicates that the dimension will be allocated;
    - {i \[\]} that indicates that the dimension will not be allocated.

    For example, the following code creates a two-dimensional array, but
    only the first dimension of the array is allocated and initialized:
    {[
      let arr = make_array "byte[_][]" 2l
    ]}

    Raises [Java_exception] if a dimension is negative. *)


(** {6 Method call} *)

external call : 'a java_method -> 'a =
  "java method call"
(** [call "methoddesc" p0 ... pn] calls and returns the result of method
    [methoddesc] called with parameters [pi], where [p0] is the instance
    to call method upon if the method is not static. For example, the
    following code compares strings s1 and s2:
    {[
      let cmp = call "java.lang.String.compareTo(java.lang.String):int" s1 s2
    ]}

    Raises [Java_exception] if the method throws an exception. *)


(** {6 Field access} *)

external get : 'a java_field_get -> 'a =
  "java field get"
(** [get "fielddesc" obj] retrieves the value of field [fielddesc] for
    instance [obj]. The [obj] value should not be replaced by [()] if
    [fielddesc] designates a static field. For example, the following
    code gets the maximum value of a integer:
    {[
      let max_int = get "java.lang.Integer.MAX_VALUE:int" ()
    ]} *)

external set : 'a java_field_set -> 'a =
  "java field set"
(** [set "fielddesc" obj x] changes the value of field [fielddesc] for
    instance [obj] to [x]. The [obj] value should not be provided if
    [fielddesc] designates a static field. For example, the following
    code sets the height of a dimension to zero:
    {[
      let () = set "java.awt.Dimension.height:int" dim 0l
    ]} *)


(** {6 Null value} *)

val null : 'a java_instance
(** The {i null} value. *)

external is_null : 'a java_instance -> bool =
  "java is_null"
(** [is_null x] returns [true] iff [x] is equal to {i null}. *)

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"
(** [is_not_null x] returns [false] iff [x] is equal to {i null}. *)


(** {6 Equality test} *)

external equal : 'a java_instance -> 'b java_instance -> bool =
  "java =="
(** [equal x y] returns [true] if [x] and [y] designate the very same
    reference. *)

external not_equal : 'a java_instance -> 'b java_instance -> bool =
  "java !="
(** [not_equal x y] returns [false] if [x] and [y] designate the very
    same reference. *)


(** {6 Class test} *)

external instanceof : 'a java_reference_type -> 'b java_instance -> bool =
  "java instanceof"
(** [instanceof "classname" x] returns [true] if [x] is an instance of
    [classname] or one of its subclasses, where [classname] can designate
    an array type. *)

external cast : 'a java_reference_type -> 'b java_instance -> 'a =
  "java cast"
(** [cast "classname" x] casts [x] to an instance of [classname], where
    [classname] can designate an array type.

    Raises [Java_exception] if the cast fails. *)


(** {6 Class retrieval} *)

external get_class : 'a java_any_type -> java'lang'Class java_instance =
  "java class"
(** [get_class "class_or_primitive_name"] returns the class instance
    representing the passed type descriptor. *)


(** {6 Exception throw} *)

external throw : java'lang'Throwable java_extends -> 'a =
  "java throw"
(** [throw x] raises the exception instance [x]
    (wrapped into a [Java_exception] on the OCaml side). *)


(** {6 Synchronization} *)

external synchronized : 'a java_instance -> (unit -> unit) -> unit =
  "java synchronized"
(** [synchronized obj (fun () -> ...)] is equivalent to the Java code
    {i synchronized (obj) \{ ... \}}.

    Raises [Java_exception] if the [obj] is {i null}. *)


(** {6 Interface implementation} *)

external proxy_loader : 'a java_proxy -> java'lang'ClassLoader java_extends -> 'a =
  "java proxy loader"
(** [proxy_loader "interfacenames" cl impl] returns an instance that
    implements [interfacenames] (a comma-separated list of interface
    names) using the methods provided by [impl]. The class is defined in
    the class loader [cl].

    For example, an instance of {i java.lang.Runnable} can be built using
    the following code:
    {[
      proxy "java.lang.Runnable" (object
        method run = ...
      end)
    ]}

    When only one interface is provided, the instance returned by [proxy]
    has this type, otherwise it has type {i java.lang.Object}.

    It is also possible to override methods from the {i java.lang.Object}
    class (independently of their presence/absence in any of the
    interfaces). To this end, it is possible to use the {i .methodName}
    notation in the interface list. As of Java 1.7, only three methods
    can be overridden: {i toString}, {i equals}, and {i hashCode}. For
    example, an instance of {i java.lang.Runnable} overriding {i toString}
    can be built using the following code:
    {[
      proxy "java.lang.Runnable, .toString" (object
        method run = ...
        method toString = ...
      end)
    ]}

 *)

external proxy_system : 'a java_proxy -> 'a =
  "java proxy system"
(** Same as [proxy_loader], but uses the system class loader. *)

external proxy_runtime : 'a java_proxy -> 'a =
  "java proxy runtime"
(** Same as [proxy_loader], but uses the class loader that was used to
    load the OCaml-Java runtime. *)

external proxy : 'a java_proxy -> 'a =
  "java proxy system"
(** Sysnonym for [proxy_system]. *)


(** {6 Miscellaneous} *)

val wrap : 'a java_instance -> 'a java_instance option
(** [wrap x] wraps the reference [x] into an option type:
    - [Some x] if [x] is not null;
    - [None] if [x] is null. *)
