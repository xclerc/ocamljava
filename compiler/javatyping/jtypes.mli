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

(** Typing for OCaml-Java extensions. *)


(** {6 Utilities} *)

val generics_not_available : unit -> 'a
(** Raises a fatal error indicating that generics are not available. *)

val is_instance_or_extends : Types.type_expr -> bool
(** Tests whether the passed type is either [... java_instance] or
    [... java_extends]. *)

val is_special_primitive : string -> bool
(** Tests whether the passed primitive name designates a {i special} one,
    {i i.e.} one that takes as its first parameter a literal format-string
    describing the referenced Java element that should be erased. *)

val use_dots : string -> string
(** [use_dots s] returns a copy of [s] where all single quotes have been
    replaced by dots. *)

val use_single_quotes : string -> string
(** [use_single_quotes s] returns a copy of [s] where all dots and
    dollars have been replaced by single quotes. *)


(** {6 Packages} *)

val reset_opened_packages : unit -> unit
(** Reset the list of opened packages. *)

val open_package : string -> Location.t -> unit
(** [open_package pack loc] indicates that package [pack] is opened at
    the source point [loc]. *)


(** {6 Conversions between classes and tags} *)

val tags_of_class : string -> Location.t -> string list
(** [tags_of_class class_name location] converts the class whose name is
    [class_names] into a set of tags representing all its parents classes
    and interfaces. The [location] parameter is usedto handle opened
    packages.

    Raises [Not_found] if the class whose name is passed cannot be found.

    Raises [Failure "visibility"] if the passed class name is ambiguous.

    Raises [Failure "ambiguous"] if the passed class name is ambiguous. *)

val classes_of_tags : string list -> string list
(** [classes_of_tags tags] converts the passed set of tags into the list
    of classes that would be needed to generate such a set if using
    [tags_of_class].

    Raises [Lookup.Exception] if a class cannot be loaded. *)


(** {6 Conversion from literal format-strings to typing information} *)

type identifier = int
(** The type of identifiers for typing infos. The [int] value is used to
    easily store a reference in an AST/lambda/... representation.

    The mapping from the [int] value to the actuel typing information
    being stored inside this module. *)

type conversion_function = (Types.type_desc -> Types.type_expr) -> string -> Location.t -> Types.type_expr * identifier
(** The type of functions converting format-strings to typing
    information, where parameters are:
    - type creation function (typically [Ctype.newty]);
    - format-string to convert;
    - format-string location;
    and returned elements:
    - type expression for the passed string;
    - identifier to retrieved the complete typing information.

    The function is expected to raise [Failure] to indicate that
    conversion failed. *)

(** {7 Constructors} *)

type constructor_info = {
    constructor_class : BaristaLibrary.ClassDefinition.t;
    constructor_method : BaristaLibrary.Method.constructor;
    constructor_ellipsis : bool;
  }
(** The typing information associated to a constructor. *)

val java_constructor_of_string : conversion_function
(** Conversion function for constructor invocations. *)

val get_constructor_info : identifier -> constructor_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_constructor_of_string]. *)

(** {7 Arrays} *)

type array_info = {
    array_type : BaristaLibrary.Descriptor.array_type;
    array_total_dimensions : int;
    array_init_dimensions : int;
  }
(** The typing information associated to an array. *)

val java_array_shape_of_string : bool -> conversion_function
(** Conversion function for array creations. *)

val get_array_info : identifier -> array_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_array_shape_of_string]. *)

(** {7 Methods} *)

type method_call =
  | Bare_call (** Simply calls the method. *)
  | Pop_result (** Calls the method, and discards its result if any. *)
  | Push_instance (** Calls the method, and pushed the instance if no result. *)
(** The kind of method calls. *)

type method_info = {
    method_class : BaristaLibrary.ClassDefinition.t;
    method_method : BaristaLibrary.Method.regular;
    method_call : method_call;
    method_ellipsis : bool;
  }
(** The typing information associated to a method. *)

val java_method_of_string : method_call -> conversion_function
(** Conversion function for method invocations. *)

val get_method_info : identifier -> method_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_method_of_string]. *)

(** {7 Fields} *)

type field_info = {
    field_class : BaristaLibrary.ClassDefinition.t;
    field_field : BaristaLibrary.Field.t;
  }
(** The typing information associated to a field. *)

val java_field_get_of_string : conversion_function
(** Conversion function for field read access. *)

val java_field_set_of_string : conversion_function
(** Conversion function for field write access. *)

val get_field_get_info : identifier -> field_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_field_get_of_string]. *)

val get_field_set_info : identifier -> field_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_field_set_of_string]. *)

(** {7 Dynamic type checks} *)

type type_info = {
    type_class : BaristaLibrary.Descriptor.non_void_java_type;
  }
(** The typing information associated to a type. *)

val java_reference_type_of_string : conversion_function
(** Conversion function for dynamic type checks/casts. *)

val get_reference_type_info : identifier -> type_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_reference_type_of_string]. *)

(** {7 Type information} *)

type any_type_info = {
    any_type_desc : BaristaLibrary.Descriptor.java_type;
  }
(** The typing information associated to a type descriptor. *)

val java_any_type_of_string : conversion_function
(** Conversion function for dynamic retrieving the class associated with
    a type descriptor. *)

val get_any_type_info : identifier -> any_type_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_any_type_of_string]. *)

(** {7 Proxies} *)

type proxy_info = {
    proxy_class : BaristaLibrary.Name.for_class;
    proxy_classes : BaristaLibrary.ClassDefinition.t list;
    proxy_mapping : (string * string * BaristaLibrary.Descriptor.for_method * string) list;
  }
(** The typing information associated to a proxy. *)

val java_proxy_of_string : (Types.type_expr -> Types.type_expr) -> conversion_function
(** Conversion function for proxy creations. *)

val get_proxy_info : identifier -> proxy_info
(** Retrieves the typing formation associated with the passed identifier.
    The passed identifier should have been created by
    [java_constructor_of_string]. *)

(** {7 Miscellaneous} *)

val get_arity : string -> int -> int
(** [get_arity prim_name id] returns the number of parameters actually
    expected by primitive [prim_name] with typing information identified
    by [id]. *)
