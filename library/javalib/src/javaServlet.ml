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


(* Generic servlets *)

type generic = javax'servlet'GenericServlet java_instance

type request = javax'servlet'ServletRequest java_instance

type response = javax'servlet'ServletResponse java_instance

module type Generic = sig
  type t
  val init    : generic -> t
  val service : t -> generic -> request -> response -> unit
  val destroy : t -> generic -> unit
end

module Default_Generic = struct
  let service _ _ _ _ = ()
  let destroy _ _     = ()
end


(* HTTP servlets *)

type http = javax'servlet'http'HttpServlet java_instance

type http_request = javax'servlet'http'HttpServletRequest java_instance

type http_response = javax'servlet'http'HttpServletResponse java_instance

module type HTTP = sig
  type t
  val init              : http -> t
  val do_delete         : t -> http -> http_request -> http_response -> unit
  val do_get            : t -> http -> http_request -> http_response -> unit
  val do_head           : t -> http -> http_request -> http_response -> unit
  val do_options        : t -> http -> http_request -> http_response -> unit
  val do_post           : t -> http -> http_request -> http_response -> unit
  val do_put            : t -> http -> http_request -> http_response -> unit
  val do_trace          : t -> http -> http_request -> http_response -> unit
  val get_last_modified : t -> http -> http_request -> int64
  val destroy           : t -> http -> unit
end

let options resp l =
  l
  |> List.map
      (function
        | `GET     -> !@"GET"
        | `HEAD    -> !@"HEAD"
        | `POST    -> !@"POST"
        | `PUT     -> !@"PUT"
        | `DELETE  -> !@"DELETE"
        | `TRACE   -> !@"TRACE"
        | `OPTIONS -> !@"OPTIONS")
  |> JavaString.concat !@", "
  |> Java.call "javax.servlet.http.HttpServletResponse.setHeader(String,String)"
      resp !@"Allow"

module Default_HTTP = struct
  let error meth req resp =
    let protocol = Java.call "javax.servlet.http.HttpServletRequest.getProtocol()" req in
    let code =
      if Java.call "String.endsWith(String)" protocol !@"1.1" then
        Java.get "javax.servlet.http.HttpServletResponse.SC_METHOD_NOT_ALLOWED" ()
      else
        Java.get "javax.servlet.http.HttpServletResponse.SC_BAD_REQUEST" () in
    Printf.sprintf "HTTP method %s is not supported by this URL" meth
    |> JavaString.of_string
    |> Java.call "javax.servlet.http.HttpServletResponse.sendError(int,String)" resp code
  let do_delete _ _ req resp = error "delete" req resp
  let do_get    _ _ req resp = error "get"    req resp
  let do_head   _ _ req resp = error "head"   req resp
  let do_post   _ _ req resp = error "post"   req resp
  let do_put    _ _ req resp = error "put"    req resp
  let do_trace _ _ req resp =
    let append buff x =
      Java.exec "StringBuilder.append(String):StringBuilder" buff x in
    let buff = Java.make "StringBuilder(String)" !@"TRACE " in
    append buff (Java.call "javax.servlet.http.HttpServletRequest.getRequestURI()" req);
    append buff !@" ";
    append buff (Java.call "javax.servlet.http.HttpServletRequest.getProtocol()" req);
    let headers = Java.call "javax.servlet.http.HttpServletRequest.getHeaderNames()" req in
    while Java.call "java.util.Enumeration.hasMoreElements()" headers do
      let header_name =
        Java.call "java.util.Enumeration.nextElement()" headers
        |> Java.cast "String" in
      let header_value =
        Java.call "javax.servlet.http.HttpServletRequest.getHeader(String)"
          req header_name in
      append buff !@"\r\n";
      append buff header_name;
      append buff !@": ";
      append buff header_value
    done;
    append buff !@"\r\n";
    let length = Java.call "StringBuilder.length()" buff in
    Java.call "javax.servlet.http.HttpServletResponse.setContentType(String)" resp !@"message/http";
    Java.call "javax.servlet.http.HttpServletResponse.setContentLength(int)" resp length;
    let out = Java.call "javax.servlet.http.HttpServletResponse.getOutputStream()" resp in
    buff
    |> Java.call "StringBuilder.toString()"
    |> Java.call "javax.servlet.ServletOutputStream.print(String)" out;
    Java.call "java.io.OutputStream.close()" out
  let get_last_modified _ _ _ = -1L
  let destroy _ _ = ()
end


(* Listeners *)

type servlet_context_event = javax'servlet'ServletContextEvent java_instance

type servlet_context_attribute_event = javax'servlet'ServletContextAttributeEvent java_instance

type http_session_event = javax'servlet'http'HttpSessionEvent java_instance

type http_session_binding_event = javax'servlet'http'HttpSessionBindingEvent java_instance

module type ServletContextListener = sig
  val context_initialized : servlet_context_event -> unit
  val context_destroyed   : servlet_context_event -> unit
end

module type ServletContextAttributeListener = sig
  val attribute_added    : servlet_context_attribute_event -> unit
  val attribute_removed  : servlet_context_attribute_event -> unit
  val attribute_replaced : servlet_context_attribute_event -> unit
end

module type HTTPSessionListener = sig
  val session_created   : http_session_event -> unit
  val session_destroyed : http_session_event -> unit
end

module type HTTPSessionActivationListener = sig
  val session_did_activate   : http_session_event -> unit
  val session_will_passivate : http_session_event -> unit
end

module type HTTPSessionAttributeListener = sig
  val attribute_added    : http_session_binding_event -> unit
  val attribute_removed  : http_session_binding_event -> unit
  val attribute_replaced : http_session_binding_event -> unit
end

module type HTTPSessionBindingListener = sig
  val value_bound   : http_session_binding_event -> unit
  val value_unbound : http_session_binding_event -> unit
end

module type HTTPSessionIdListener = sig
  val session_id_changed : http_session_event -> JavaString.t -> unit
end
