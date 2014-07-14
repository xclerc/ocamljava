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

(** Support for Java servlets.

    In order to produce a class to be used as either a servlet or a
    servlet listener, it is necessary to compile the module with one of
    the following command-line switches:
    - {i -servlet generic};
    - {i -servlet http};
    - {i -servlet context-listener};
    - {i -servlet context-attribute-listener};
    - {i -servlet session-listener};
    - {i -servlet session-activation-listener};
    - {i -servlet session-attribute-listener};
    - {i -servlet session-binding-listener};
    - {i -servlet session-id-listener}.

    The compiled module has to abide (respectively) the following module
    type:
    - [Generic];
    - [HTTP];
    - [ServletContextListener];
    - [ServletContextAttributeListener];
    - [HTTPSessionListener];
    - [HTTPSessionActivationListener];
    - [HTTPSessionAttributeListener];
    - [HTTPSessionBindingListener];
    - [HTTPSessionIdListener].

    When a module {i M} is compiled with a command-line switch
    {i -servlet x}, an additional class with name {i MImpl} is produced.
    The {i MImpl} class implements (or extends) the interface (or class)
    related to {i x}. As an example, compiling with {i -servlet http}
    with result in a class extending {i javax.servlet.http.HttpServlet}. *)


(** {6 Generic servlets} *)

type generic = javax'servlet'GenericServlet java_instance
(** Shorthand for the type of generic servlets. *)

type request = javax'servlet'ServletRequest java_instance
(** Shorthand for the type of requests to generic servlets. *)

type response = javax'servlet'ServletResponse java_instance
(** Shorthand for the type of responses to generic servlets. *)

module type Generic = sig
  type t
  (** The type of values stored by servlets. *)
  val init : generic -> t
  (** Initializes a new servlet by returning its value (the function is
      executed by {i javax.servlet.GenericServlet.init()}). *)
  val service : t -> generic -> request -> response -> unit
  (** [service v inst req resp] handles the request [req] through the
      servlet instance [inst] (associated to value [v]). The parameter
      [resp] is used to send the response. *)
  val destroy : t -> generic -> unit
  (** Called by the servlet container when the servlet is being taken out
      of service. *)
end
(** The module type for servlets compiled with {i -servlet generic}. *)

module Default_Generic : sig
  val service : 'a -> generic -> request -> response -> unit
  val destroy : 'a -> generic -> unit
end
(** Default ({i i.e.} empty) implementation for generic servlets. *)


(** {6 HTTP servlets} *)

type http = javax'servlet'http'HttpServlet java_instance
(** Shorthand for the type of HTTP servlets. *)

type http_request = javax'servlet'http'HttpServletRequest java_instance
(** Shorthand for the type of requests to HTTP servlets. *)

type http_response = javax'servlet'http'HttpServletResponse java_instance
(** Shorthand for the type of responses to HTTP servlets. *)

module type HTTP = sig
  type t
  (** The type of values stored by servlets. *)
  val init : http -> t
  (** Initializes a new servlet by returning its value (the function is
      executed by {i javax.servlet.http.HttpServlet.init()}). *)
  val do_delete : t -> http -> http_request -> http_response -> unit
  (** [do_delete v inst req resp] handles the {i DELETE} request [req]
      through the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val do_get : t -> http -> http_request -> http_response -> unit
  (** [do_get v inst req resp] handles the {i GET} request [req] through
      the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val do_head : t -> http -> http_request -> http_response -> unit
  (** [do_head v inst req resp] handles the {i HEAD} request [req]
      through the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val do_options : t -> http -> http_request -> http_response -> unit
  (** [do_options v inst req resp] handles the {i OPTIONS} request [req]
      through the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val do_post : t -> http -> http_request -> http_response -> unit
  (** [do_post v inst req resp] handles the {i POST} request [req]
      through the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val do_put : t -> http -> http_request -> http_response -> unit
  (** [do_put v inst req resp] handles the {i PUT} request [req] through
      the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val do_trace : t -> http -> http_request -> http_response -> unit
  (** [do_trace v inst req resp] handles the {i TRACE} request [req]
      through the servlet instance [inst] (associated to value [v]). The
      parameter [resp] is used to send the response. *)
  val get_last_modified : t -> http -> http_request -> int64
  (** [get_last_modified v inst req] handles the request [req] through
      the servlet instance [inst] (associated to value [v]), returning
      the time the underlying information was last modified (in
      milliseconds since {i 1970-01-01}), or a negative value if the time
      is unknown. *)
  val destroy : t -> http -> unit
  (** Called by the servlet container when the servlet is being taken out
      of service. *)
end
(** The module type for servlets compiled with {i -servlet http}. *)

val options : http_response -> [`GET | `HEAD | `POST | `PUT | `DELETE | `TRACE | `OPTIONS] list -> unit
(** [options resp l] is an implementation of [do_options _ _ _ resp]
    where [l] is the list of methods supported by the servlet. *)

module Default_HTTP : sig
  val do_delete : 'a -> http -> http_request -> http_response -> unit
  val do_get : 'a -> http -> http_request -> http_response -> unit
  val do_head : 'a -> http -> http_request -> http_response -> unit
  val do_post : 'a -> http -> http_request -> http_response -> unit
  val do_put : 'a -> http -> http_request -> http_response -> unit
  val do_trace : 'a -> http -> http_request -> http_response -> unit
  val get_last_modified : 'a -> http -> http_request -> int64
  val destroy : 'a -> http -> unit
end
(** Default ({i i.e.} empty) implementation for HTTP servlets. *)


(** {6 Listeners} *)

module type ServletContextListener = sig
  val context_initialized : javax'servlet'ServletContextEvent java_instance -> unit
  (** Called to notify that the application initialization is starting. *)
  val context_destroyed : javax'servlet'ServletContextEvent java_instance -> unit
  (** Called to notify that the servlet context will shutdown. *)
end
(** The module type for listeners compiled with {i -servlet context-listener}. *)

module type ServletContextAttributeListener = sig
  val attribute_added : javax'servlet'ServletContextAttributeEvent java_instance -> unit
  (** Called to notify that a new attribute was added to the context. *)
  val attribute_removed : javax'servlet'ServletContextAttributeEvent java_instance -> unit
  (** Called to notify that an attribute was removed from the context. *)
  val attribute_replaced : javax'servlet'ServletContextAttributeEvent java_instance -> unit
  (** Called to notify that an attribute was replaced in the context. *)
end
(** The module type for listeners compiled with {i -servlet context-attribute-listener}. *)

module type HTTPSessionListener = sig
  val session_created : javax'servlet'http'HttpSessionEvent java_instance -> unit
  (** Called to notify that a session was created. *)
  val session_destroyed : javax'servlet'http'HttpSessionEvent java_instance -> unit
  (** Called to notify that a session is about to be invalidated. *)
end
(** The module type for listeners compiled with {i -servlet session-listener}. *)

module type HTTPSessionActivationListener = sig
  val session_did_activate : javax'servlet'http'HttpSessionEvent java_instance -> unit
  (** Called to notify that a session has just been activated. *)
  val session_will_passivate : javax'servlet'http'HttpSessionEvent java_instance -> unit
  (** Called to notify that a session is about to be passivated. *)
end
(** The module type for listeners compiled with {i -servlet session-activation-listener}. *)

module type HTTPSessionAttributeListener = sig
  val attribute_added : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  (** Called to notify that a new session attribute has been added to a session. *)
  val attribute_removed : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  (** Called to notify that a session attribute has been removed from a session. *)
  val attribute_replaced : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  (** Called to notify that a session attribute has been replaced in a session. *)
end
(** The module type for listeners compiled with {i -servlet session-attribute-listener}. *)

module type HTTPSessionBindingListener = sig
  val value_bound : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  (** Called to notify that an object is being bound to a session as its identifier. *)
  val value_unbound : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  (** Called to notify that an object is being unbound from a session. *)
end
(** The module type for listeners compiled with {i -servlet session-binding-listener}. *)

module type HTTPSessionIdListener = sig
  val session_id_changed : javax'servlet'http'HttpSessionEvent java_instance -> java'lang'String java_instance -> unit
  (** Called to notify that the session identifier has changed. *)
end
(** The module type for listeners compiled with {i -servlet session-id-listener}. *)
