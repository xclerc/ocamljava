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
  val init : generic -> t
  val service : t -> generic -> request -> response -> unit
  val destroy : t -> generic -> unit
end

module Default_Generic = struct
  let service _ _ _ _ = ()
  let destroy _ _ = ()
end


(* HTTP servlets *)

type http = javax'servlet'http'HttpServlet java_instance

type http_request = javax'servlet'http'HttpServletRequest java_instance

type http_response = javax'servlet'http'HttpServletResponse java_instance

module type HTTP = sig
  type t
  val init : http -> t
  val do_delete : t -> http -> http_request -> http_response -> unit
  val do_get : t -> http -> http_request -> http_response -> unit
  val do_head : t -> http -> http_request -> http_response -> unit
  val do_options : t -> http -> http_request -> http_response -> unit
  val do_post : t -> http -> http_request -> http_response -> unit
  val do_put : t -> http -> http_request -> http_response -> unit
  val do_trace : t -> http -> http_request -> http_response -> unit
  val get_last_modified : t -> http -> http_request -> int64
  val destroy : t -> http -> unit
end

let allow = JavaString.of_string "Allow"

let options resp l =
  l
  |> List.map
      (function
        | `GET     -> "GET"
        | `HEAD    -> "HEAD"
        | `POST    -> "POST"
        | `PUT     -> "PUT"
        | `DELETE  -> "DELETE"
        | `TRACE   -> "TRACE"
        | `OPTIONS -> "OPTIONS")
  |> String.concat ", "
  |> JavaString.of_string
  |> Java.call "javax.servlet.http.HttpServletResponse.setHeader(_,_)" resp allow

module Default_HTTP = struct
  let v_1_1 = JavaString.of_string "1.1"
  let cr_lf = JavaString.of_string "\r\n"
  let colon = JavaString.of_string ": "
  let trace = JavaString.of_string "TRACE "
  let space = JavaString.of_string " "
  let message_http = JavaString.of_string "message/http"
  let error meth req resp =
    let protocol = Java.call "javax.servlet.http.HttpServletRequest.getProtocol()" req in
    let code =
      if Java.call "String.endsWith(_)" protocol v_1_1 then
        Java.get "javax.servlet.http.HttpServletResponse.SC_METHOD_NOT_ALLOWED" ()
      else 
        Java.get "javax.servlet.http.HttpServletResponse.SC_BAD_REQUEST" () in
    let msg = Printf.sprintf "HTTP method %s is not supported by this URL" meth |> JavaString.of_string in
    Java.call "javax.servlet.http.HttpServletResponse.sendError(_,_)" resp code msg
  let do_delete _ _ req resp = error "delete" req resp
  let do_get _ _ req resp = error "get" req resp
  let do_head _ _ req resp = error "head" req resp
  let do_post _ _ req resp = error "post" req resp
  let do_put _ _ req resp = error "put" req resp
  let do_trace _ _ req resp =
    let append buff x =
      Java.call "StringBuilder.append(String):StringBuilder" buff x
      |> ignore in
    let buff = Java.make "StringBuilder(String)" trace in
    append buff (Java.call "javax.servlet.http.HttpServletRequest.getRequestURI()" req);
    append buff space;
    append buff (Java.call "javax.servlet.http.HttpServletRequest.getProtocol()" req);
    let headers = Java.call "javax.servlet.http.HttpServletRequest.getHeaderNames()" req in
    while Java.call "java.util.Enumeration.hasMoreElements()" headers do
      let header_name =
        Java.call "java.util.Enumeration.nextElement()" headers
        |> Java.cast "String" in      
      let header_value = Java.call "javax.servlet.http.HttpServletRequest.getHeader(_)" req header_name in
      append buff cr_lf;
      append buff header_name;
      append buff colon;
      append buff header_value
    done;
    append buff cr_lf;
    let length = Java.call "StringBuilder.length()" buff in
    Java.call "javax.servlet.http.HttpServletResponse.setContentType(_)" resp message_http;
    Java.call "javax.servlet.http.HttpServletResponse.setContentLength(_)" resp length;
    let out = Java.call "javax.servlet.http.HttpServletResponse.getOutputStream()" resp in
    buff
    |> Java.call "StringBuilder.toString()"
    |> Java.call "javax.servlet.ServletOutputStream.print(String)" out
  let get_last_modified _ _ _ = -1L
  let destroy _ _ = ()
end


(* Listeners *)

module type ServletContextListener = sig
  val context_initialized : javax'servlet'ServletContextEvent java_instance -> unit
  val context_destroyed : javax'servlet'ServletContextEvent java_instance -> unit
end

module type ServletContextAttributeListener = sig
  val attribute_added : javax'servlet'ServletContextAttributeEvent java_instance -> unit
  val attribute_removed : javax'servlet'ServletContextAttributeEvent java_instance -> unit
  val attribute_replaced : javax'servlet'ServletContextAttributeEvent java_instance -> unit
end

module type HTTPSessionListener = sig
  val session_created : javax'servlet'http'HttpSessionEvent java_instance -> unit
  val session_destroyed : javax'servlet'http'HttpSessionEvent java_instance -> unit
end

module type HTTPSessionActivationListener = sig
  val session_did_activate : javax'servlet'http'HttpSessionEvent java_instance -> unit
  val session_will_passivate : javax'servlet'http'HttpSessionEvent java_instance -> unit
end

module type HTTPSessionAttributeListener = sig
  val attribute_added : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  val attribute_removed : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  val attribute_replaced : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
end

module type HTTPSessionBindingListener = sig
  val value_bound : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
  val value_unbound : javax'servlet'http'HttpSessionBindingEvent java_instance -> unit
end

module type HTTPSessionIdListener = sig
  val session_id_changed : javax'servlet'http'HttpSessionEvent java_instance -> java'lang'String java_instance -> unit
end
