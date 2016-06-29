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

(** Manipulation of Java references.

    The {k ocamljava} compiler allows to manipulate Java references
    through typer extensions triggered by the {k -java-extensions}
    command-line switch. The Java types can then be manipulated through
    two types, namely [cn java_instance] and [cn java_extends]. The
    former is used to designate instances of exactly the given class
    [cn], while the latter is used to designate instances of either the
    class [cn] or any of its subclasses. In both cases, the type
    parameter [cn] is used to specify the class, and uses the
    fully-qualified name of the class where dots are replaced with single
    quotes. As a consequence, the type of Java strings is thus written
    [java'lang'String java_instance].

    This module contains the functions needed to create new instances,
    call methods, and access fields. Such functions use {i descriptors}
    to determine the constructor/method/field to use. These descriptors
    are written as string ltterals; for example, parsing a string into an
    integer can be done by writing:
    {[
      let i = call "java.lang.Integer.parseInt(java.lang.String):int" s
    ]}
    However, it is possible to use simple names rather than qualified
    names for classes if their packages have been opened. Initially, only
    the [java.lang] package is opened. A package can be opened though a
    modified [open Package'packname] directive; for example, the
    [javax.awt] package can be opened by writing:
    {[
      open Package'java'awt
    ]}
    A single class can be opened through a modified [open Class'classname]
    directive ; for example, the [javax.awt.Frame] class can be opened by
    writing:
    {[
      open Class'java'awt'Frame
    ]}
    It is also possible to take advantage of opened packages in types, by
    replacing the package name with an underscore. The type of Java
    strings can thus be written [_'String java_instance].

    Moreover, types of fields and return types of methods can be elided
    as long as there is no ambiguity. Types of method and constructor
    parameters can also be replaced with single underscores, leading to
    the shorter code to parse an integer:
    {[
      let i = call "Integer.parseInt(_)" s
    ]}
    Furthermore, it is possible to use dashes to denote any number of
    parameters, leading to the lightest code to rotate an integer value:
    {[
      let j = call "Integer.rotateLeft(-)" i nbits
    ]}
    The compiler will issue an error if there is an ambiguity.

    Besides the [cn java_instance] and [cn java_extends] types that are
    used to map Java reference types, the compiler maps Java primitive
    type to newly-introduced OCaml types that are synonym of OCaml
    predefined types. The complete mapping is given by the following
    table.
    {C {table {caption Mapping of Java primitive types.}
              {row {header Java type}
                   {header OCaml type}
                   {header Synonym}}
              {row {data [boolean]}
                   {data [java_boolean]}
                   {data [bool]}}
              {row {data [byte]}
                   {data [java_byte]}
                   {data [int]}}
              {row {data [char]}
                   {data [java_char]}
                   {data [int]}}
              {row {data [double]}
                   {data [java_double]}
                   {data [float]}}
              {row {data [float]}
                   {data [java_float]}
                   {data [float]}}
              {row {data [int]}
                   {data [java_int]}
                   {data [int32]}}
              {row {data [long]}
                   {data [java_long]}
                   {data [int64]}}
              {row {data [short]}
                   {data [java_short]}
                   {data [int]}}
              {row {data [void]}
                   {data [java_void]}
                   {data [unit]}}}}

    A similar scheme is used for Java array types, with a dedicated type
    for each primitive array type, and an additionnal type for arrays of
    references. However, contrary to primitive types, array types are not
    synonyms of existing OCaml types, and a module is associated to each
    type in order to provide usual operations. The complete mapping from
    Java array types to OCaml types is given by the following table.
    {C {table {caption Mapping of Java array types.}
              {row {header Java type}
                   {header OCaml type}
                   {header OCaml module}}
              {row {data [boolean[]]}
                   {data [java_boolean java_boolean_array]}
                   {data [JavaBooleanArray]}}
              {row {data [byte[]]}
                   {data [java_byte java_byte_array]}
                   {data [JavaByteArray]}}
              {row {data [char[]]}
                   {data [java_char java_char_array]}
                   {data [JavaCharArray]}}
              {row {data [double[]]}
                   {data [java_double java_double_array]}
                   {data [JavaDoubleArray]}}
              {row {data [float[]]}
                   {data [java_float java_float_array]}
                   {data [JavaFloatArray]}}
              {row {data [int[]]}
                   {data [java_int java_int_array]}
                   {data [JavaIntArray]}}
              {row {data [long[]]}
                   {data [java_long java_long_array]}
                   {data [JavaLongArray]}}
              {row {data [short[]]}
                   {data [java_short java_short_array]}
                   {data [JavaShortArray]}}
              {row {data [reference[]]}
                   {data ['a java_reference_array]}
                   {data [JavaReferenceArray]}}}}
    Each primitive type is parametrized by the type of its elements,
    allowing a generic treatment of all array types through the
    [JavaArray] module.

    When calling a method with a variable number of arguments, two
    variants are accepted for the descriptor, impacting the way arguments
    should be passed:
    - if ["C.m(T[])"] is used, then the arguments are passed through a
      Java array;
    - if ["C.m(T...)"] is used, then the arguments are passed through an
      OCaml literal array.

    The following code illustrates the alternative:
    {[
      let l1 =
        Java.call "Arrays.asList(Object[])"
          (Java.make_array "Object[]" 5l)
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
    The former is used for exceptions whose instances inherit from
    {java java.lang.Exception} ({i i.e. checked} exceptions), while the
    latter is used for exceptions whose instances inherit from
    {java java.lang.Error} ({i i.e. unchecked} exceptions). *)


(** {6 Instance creation} *)

external make : 'a java_constructor -> 'a =
  "java constructor"
(** [make desc param1 ... paramn] calls the constructor whose descriptor
    is [desc] with parameters [param1 ... paramn], and returns the
    created instance.

    [desc] is composed of the following elements:
    - a class name;
    - an opening parenthesis ({i i.e.} [(]);
    - a comma-separated list of types;
    - a closing parenthesis ({i i.e.} [)]).

    For example, the following code creates an object instance:
    {[
      let inst = make "java.lang.Object()" ()
    ]}

    @raise Java_exception if the constructor throws an exception
    @raise Java_error if the constructor throws an error *)

external make_array : 'a java_array_shape -> 'a =
  "java make array"
(** [make_array desc dim1 ... dimn] builds and returns an array, whose
    number of dimensions and type of elements are determined by [desc].
    Each element is initialized to the default value for the type (that
    is [false] for booleans, zero for other primitive types, and [null]
    for reference types).

    [desc] is composed of the following elements:
    - a type;
    - a non-empty list of [[]] characters.

    For example, the following code creates a 2x3 matrix of byte values:
    {[
      let arr = make_array "byte[][]" 2l 3l
    ]}

    @raise Java_exception if a dimension is negative *)

external make_array_dims : 'a java_array_shape_dims -> 'a =
  "java make array dims"
(** [make_array_dims desc dim1 ... dimn] is similar to {!make_array},
    except that the array descriptor is made of two kinds of dimension
    specifiers, allowing to initialize only the first dimensions of the
    array.

    [desc] is composed of the following elements:
    - a type;
    - a non-empty list of {i dimension specifiers};
    where a {i dimension specifiers} can be one of:
    - [\[_\]] that indicates that the dimension will be allocated;
    - [\[\]] that indicates that the dimension will not be allocated.

    For example, the following code creates a two-dimensional array, but
    only the first dimension of the array is allocated and initialized:
    {[
      let arr = make_array_dims "byte[_][]" 2l
    ]}

    @raise Java_exception if a dimension is negative *)


(** {6 Method call} *)

external call : 'a java_method_call -> 'a =
  "java method call"
(** [call desc param1 ... paramn] calls and returns the result of method
    [desc] called with parameters [param1 ... paramn], where [param1] is
    the instance to call method upon if the method is not static.

    [desc] is composed of the following elements:
    - a class name;
    - a dot ({i i.e.} [.]);
    - a method name;
    - an opening parenthesis ({i i.e.} [(]);
    - a comma-separated list of types;
    - a closing parenthesis ({i i.e.} [)]);
    - optionally, a colon ({i i.e.} [:]) followed by a type.

    For example, the following code compares two strings [s1] and [s2]:
    {[
      call "java.lang.String.compareTo(java.lang.String):int" s1 s2
    ]}

    @raise Java_exception if the method throws an exception
    @raise Java_error if the method throws an error *)

external exec : 'a java_method_exec -> 'a =
  "java method exec"
(** Similar to {!call}, but ignores the result if any. *)

external chain : 'a java_method_chain -> 'a =
  "java method chain"
(** Similar to {!call}, returns the instance the method was called upon. *)


(** {6 Field access} *)

external get : 'a java_field_get -> 'a =
  "java field get"
(** [get desc obj] retrieves the value of field [desc] for instance
    [obj]. The [obj] value should be replaced by [()] if [desc]
    designates a static field.

    [desc] is composed of the following elements:
    - a class name;
    - a dot ({i i.e.} [.]);
    - optionally, a colon ({i i.e.} [:]) followed by a type.

    For example, the following code gets the maximum value of a integer:
    {[
      let max_int = get "java.lang.Integer.MAX_VALUE:int" ()
    ]}

    @raise Java_exception if [obj] is [null] *)

external set : 'a java_field_set -> 'a =
  "java field set"
(** [set desc obj x] changes the value of field [desc] for
    instance [obj] to [x]. The [obj] value should not be provided if
    [desc] designates a static field.

    [desc] is composed of the following elements:
    - a class name;
    - a dot ({i i.e.} [.]);
    - optionally, a colon ({i i.e.} [:]) followed by a type.

    For example, the following code sets the height of dimension [dim] to
    zero:
    {[
      let () = set "java.awt.Dimension.height:int" dim 0l
    ]}

    @raise Java_exception if [obj] is [null] *)


(** {6 Iteration} *)

external iter : 'a java_reference_type -> ('a -> unit) -> java'util'Iterator java_extends -> unit =
  "java iter"
(** [iter desc f it] applies [f] to every element returned by [it], after
    casting them to [desc].

    [desc] is either a class name, or an array descriptor.

    @raise Java_exception if [it] is [null] *)


external fold : 'a java_reference_type -> ('b -> 'a -> 'b) -> 'b -> java'util'Iterator java_extends -> 'b =
  "java fold"
(** [fold desc f z it] computes [(f ... ((f z it0) it1) ... itn)] where
    [iti] values are successively returned by [it], and casted to [desc]
    before they are passed to [f].

    [desc] is either a class name, or an array descriptor.

    @raise Java_exception if [it] is [null] *)


(** {6 Null value} *)

val null : 'a java_instance
(** The [null] value. *)

external is_null : 'a java_instance -> bool =
  "java is_null"
(** [is_null x] returns [true] iff [x] is equal to [null]. *)

external is_not_null : 'a java_instance -> bool =
  "java is_not_null"
(** [is_not_null x] returns [false] iff [x] is equal to [null]. *)


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
(** [instanceof desc x] returns [true] if [x] is an instance of [desc].

    [desc] is either a class name, or an array descriptor. *)

external cast : 'a java_reference_type -> 'b java_instance -> 'a =
  "java cast"
(** [cast desc x] casts [x], so that it can be used as an instance of
    [desc].

    [desc] is either a class name, or an array descriptor.

    @raise Java_exception if the cast fails *)


(** {6 Class retrieval} *)

external get_class : 'a java_any_type -> java'lang'Class java_instance =
  "java class"
(** [get_class desc] returns the instance of {java java.lang.Class}
    representing the passed type descriptor.

    [desc] can designate any Java type (primitive, array, reference). *)


(** {6 Exception throw} *)

external throw : java'lang'Throwable java_extends -> 'a =
  "java throw"
(** [throw x] raises the instance [x], that will be wrapped into either
    a [Java_exception], or a [Java_error] on the OCaml side.

    @raise Java_exception if [x] is an instance of
           {java java.lang.Exception}
    @raise Java_error if [x] is an instance of {java java.lang.Error} *)


(** {6 Synchronization} *)

external synchronized : 'a java_instance -> (unit -> unit) -> unit =
  "java synchronized"
(** [synchronized obj (fun () -> ...)] is equivalent to the Java code
    [synchronized (obj) \{ ... \}].

    @raise Java_exception if the [obj] is [null] *)


(** {6 Interface implementation} *)

external proxy_loader : 'a java_proxy -> java'lang'ClassLoader java_extends -> 'a =
  "java proxy loader"
(** [proxy_loader desc cl impl] returns an instance that implements the
    interfaces specified by [desc], using the methods provided by [impl].
    The class is defined in the class loader [cl].

    [desc] is basically a comma-separated list of interface names.

    For example, an instance of {java java.lang.Runnable} can be built
    using the following code:
    {[
      proxy "java.lang.Runnable" (object
        method run = ...
      end)
    ]}
    When only one interface is provided, the instance returned has this
    type, otherwise it has type {java java.lang.Object}.

    It is also possible to override methods from the
    {java java.lang.Object} class (independently of their presence or
    absence in any of the interfaces), by using [.methodName] notation in
    [desc]. As of Java 1.7, only three methods can be overridden:
    - [toString];
    - [equals];
    - [hashCode].

    For example, an instance of {java java.lang.Runnable} overriding
    [toString] can be built using the following code:
    {[
      proxy_loader "java.lang.Runnable, .toString" loader (object
        method run = ...
        method toString = ...
      end)
    ]}

 *)

external proxy_system : 'a java_proxy -> 'a =
  "java proxy system"
(** Similar to {!proxy_loader}, but uses the system class loader. *)

external proxy_runtime : 'a java_proxy -> 'a =
  "java proxy runtime"
(** Similar to {!proxy_loader}, but uses the class loader that was used to
    load the OCaml-Java runtime. *)

external proxy : 'a java_proxy -> 'a =
  "java proxy system"
(** Synonym for {!proxy_system}. *)


(** {6 Miscellaneous} *)

val wrap : 'a java_instance -> 'a java_instance option
(** [wrap x] wraps the reference [x] into an option type:
    - [Some x] if [x] is not [null];
    - [None] if [x] is [null]. *)

val unwrap : 'a java_instance option -> 'a java_instance
(** [unwrap x] unwraps the option [x] into a bare reference:
    - [Some x] is mapped to [x];
    - [None] is mapped to [null]. *)
