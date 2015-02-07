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

(** Support for Java servlets.

    In order to compile a module as either a servlet or a servlet
    listener, it is necessary to compile the module with the
    {k -servlet {i k}} command-line switch, where {k {i k}} designates
    the kind of servlet or servlet listener. A servlet class will be
    generated, with the name {i pack.MImpl} (where {i M} is the name of
    the compiled module) where {i pack} can be set using the
    {k -java-package {i pack}} command-line switch.

    When compiling with the {k -servlet {i k}} command-line switch, the
    module has to be compatible with one of the module types of the
    [JavaServlet] module, the exact module type depending on the value of
    {k {i k}}. The following table gives the module types for the
    different applet kinds.

    {C {table {caption The various kinds of servlet.}
              {row {header parameter to {k -servlet}}
                   {header module type}}
              {row {data {k generic}}
                   {data [Generic]}}
              {row {data {k http}}
                   {data [HTTP]}}
              {row {data {k context-listener}}
                   {data [ServletContextListener]}}
              {row {data {k context-attribute-listener}}
                   {data [ServletContextAttributeListener]}}
              {row {data {k session-listener}}
                   {data [HTTPSessionListener]}}
              {row {data {k session-activation-listener}}
                   {data [HTTPSessionActivationListener]}}
              {row {data {k session-attribute-listener}}
                   {data [HTTPSessionAttributeListener]}}
              {row {data {k session-binding-listener}}
                   {data [HTTPSessionBindingListener]}}
              {row {data {k session-id-listener}}
                   {data [HTTPSessionIdListener]}}}} *)


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
      executed by {java javax.servlet.GenericServlet#init()}). *)
  val service : t -> generic -> request -> response -> unit
  (** [service v inst req resp] handles the request [req] through the
      servlet instance [inst] (associated to value [v]). The parameter
      [resp] is used to send the response. *)
  val destroy : t -> generic -> unit
  (** Called by the servlet container when the servlet is being taken out
      of service. *)
end
(** The module type for servlets compiled with {k -servlet generic}. *)

module Default_Generic : sig
  val service : 'a -> generic -> request -> response -> unit
  val destroy : 'a -> generic -> unit
end
(** Default implementation for generic servlets. *)


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
      executed by {java javax.servlet.GenericServlet#init()}). *)
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
(** The module type for servlets compiled with {k -servlet http}. *)

val options : http_response -> [`GET | `HEAD | `POST | `PUT | `DELETE | `TRACE | `OPTIONS] list -> unit
(** [options resp l] is an implementation of [do_options _ _ _ resp]
    where [l] is the list of methods supported by the servlet. *)

module Default_HTTP : sig
  val do_delete         : 'a -> http -> http_request -> http_response -> unit
  val do_get            : 'a -> http -> http_request -> http_response -> unit
  val do_head           : 'a -> http -> http_request -> http_response -> unit
  val do_post           : 'a -> http -> http_request -> http_response -> unit
  val do_put            : 'a -> http -> http_request -> http_response -> unit
  val do_trace          : 'a -> http -> http_request -> http_response -> unit
  val get_last_modified : 'a -> http -> http_request -> int64
  val destroy           : 'a -> http -> unit
end
(** Default implementation for HTTP servlets. *)


(** {6 Listeners} *)

type servlet_context_event = javax'servlet'ServletContextEvent java_instance
(** Shorthand for the type of context events. *)

type servlet_context_attribute_event = javax'servlet'ServletContextAttributeEvent java_instance
(** Shorthand for the type of context attribute events. *)

type http_session_event = javax'servlet'http'HttpSessionEvent java_instance
(** Shorthand for the type of session events. *)

type http_session_binding_event = javax'servlet'http'HttpSessionBindingEvent java_instance
(** Shorthand for the type of session binding events. *)

module type ServletContextListener = sig
  val context_initialized : servlet_context_event -> unit
  (** Called to notify that the application initialization is starting; see
      {java javax.servlet.ServletContextListener#contextInitialized(javax.servlet.ServletContextEvent)}. *)
  val context_destroyed : servlet_context_event -> unit
  (** Called to notify that the servlet context will shutdown; see
      {java javax.servlet.ServletContextListener#contextDestroyed(javax.servlet.ServletContextEvent)}. *)
end
(** The module type for listeners compiled with {k -servlet context-listener}. *)

module type ServletContextAttributeListener = sig
  val attribute_added : servlet_context_attribute_event -> unit
  (** Called to notify that a new attribute was added to the context; see
      {java javax.servlet.ServletContextAttributeListener#attributeAdded(javax.servlet.ServletContextAttributeEvent)}. *)
  val attribute_removed : servlet_context_attribute_event -> unit
  (** Called to notify that an attribute was removed from the context; see
      {java javax.servlet.ServletContextAttributeListener#attributeRemoved(javax.servlet.ServletContextAttributeEvent)}. *)
  val attribute_replaced : servlet_context_attribute_event -> unit
  (** Called to notify that an attribute was replaced in the context; see
      {java javax.servlet.ServletContextAttributeListener#attributeReplaced(javax.servlet.ServletContextAttributeEvent)}. *)
end
(** The module type for listeners compiled with {k -servlet context-attribute-listener}. *)

module type HTTPSessionListener = sig
  val session_created : http_session_event -> unit
  (** Called to notify that a session was created; see
      {java javax.servlet.http.HttpSessionListener#sessionCreated(javax.servlet.http.HttpSessionEvent)}. *)
  val session_destroyed : http_session_event -> unit
  (** Called to notify that a session is about to be invalidated; see
      {java javax.servlet.http.HttpSessionListener#sessionDestroyed(javax.servlet.http.HttpSessionEvent)}. *)
end
(** The module type for listeners compiled with {k -servlet session-listener}. *)

module type HTTPSessionActivationListener = sig
  val session_did_activate : http_session_event -> unit
  (** Called to notify that a session has just been activated; see
      {java javax.servlet.http.HttpSessionActivationListener#sessionDidActivate(javax.servlet.http.HttpSessionEvent)}. *)
  val session_will_passivate : http_session_event -> unit
  (** Called to notify that a session is about to be passivated; see
      {java javax.servlet.http.HttpSessionActivationListener#sessionWillPassivate(javax.servlet.http.HttpSessionEvent)}. *)
end
(** The module type for listeners compiled with {k -servlet session-activation-listener}. *)

module type HTTPSessionAttributeListener = sig
  val attribute_added : http_session_binding_event -> unit
  (** Called to notify that a new session attribute has been added to a
      session; see
      {java javax.servlet.http.HttpSessionAttributeListener#attributeAdded(javax.servlet.http.HttpSessionBindingEvent)}. *)
  val attribute_removed : http_session_binding_event -> unit
  (** Called to notify that a session attribute has been removed from a
      session; see
      {java javax.servlet.http.HttpSessionAttributeListener#attributeRemoved(javax.servlet.http.HttpSessionBindingEvent)}. *)
  val attribute_replaced : http_session_binding_event -> unit
  (** Called to notify that a session attribute has been replaced in a
      session; see
      {java javax.servlet.http.HttpSessionAttributeListener#attributeReplaced(javax.servlet.http.HttpSessionBindingEvent)}. *)
end
(** The module type for listeners compiled with {k -servlet session-attribute-listener}. *)

module type HTTPSessionBindingListener = sig
  val value_bound : http_session_binding_event -> unit
  (** Called to notify that an object is being bound to a session as its
      identifier; see
      {java javax.servlet.http.HttpSessionBindingListener#valueBound(javax.servlet.http.HttpSessionBindingEvent)}. *)
  val value_unbound : http_session_binding_event -> unit
  (** Called to notify that an object is being unbound from a session; see
      {java javax.servlet.http.HttpSessionBindingListener#valueUnbound(javax.servlet.http.HttpSessionBindingEvent)}. *)
end
(** The module type for listeners compiled with {k -servlet session-binding-listener}. *)

module type HTTPSessionIdListener = sig
  val session_id_changed : http_session_event -> JavaString.t -> unit
  (** Called to notify that the session identifier has changed; see
      {java javax.servlet.http.HttpSessionIdListener#sessionIdChanged(javax.servlet.http.HttpSessionEvent, java.lang.String)}. *)
end
(** The module type for listeners compiled with {k -servlet session-id-listener}. *)
