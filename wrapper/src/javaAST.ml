(*
 * This file is part of OCaml-Java wrapper.
 * Copyright (C) 2007-2014 Xavier Clerc.
 *
 * OCaml-Java wrapper is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java wrapper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

let keywords = [
  (* actual keywords *)
  "abstract"; "assert";
  "boolean"; "break"; "byte";
  "case"; "catch"; "char"; "class"; "const"; "continue";
  "default"; "do"; "double";
  "else"; "enum"; "extends";
  "final"; "finally"; "float"; "for";
  "goto";
  "if"; "implements"; "import"; "instanceof"; "int"; "interface";
  "long";
  "native"; "new";
  "package"; "private"; "protected"; "public";
  "return";
  "short"; "static"; "strictfp"; "super"; "switch"; "synchronized";
  "this"; "throw"; "throws"; "transient"; "try";
  "void"; "volatile";
  "while";
  (* constant literals *)
  "false"; "null"; "true"
]

let checked_name x =
  if List.mem x keywords then
    x ^ "$$"
  else
    x

type modifier =
  | Public
  | Protected
  | Private
  | Static
  | Abstract
  | Final
  | Native
  | Synchronized
  | Transient
  | Volatile
  | Strictfp

let string_of_modifier = function
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"
  | Static -> "static"
  | Abstract -> "abstract"
  | Final -> "final"
  | Native -> "native"
  | Synchronized -> "synchronized"
  | Transient -> "transient"
  | Volatile -> "volatile"
  | Strictfp -> "strictfp"

type type_ =
  | Byte
  | Short
  | Char
  | Int
  | Long
  | Float
  | Double
  | Boolean
  | Reference of string * (type_ list)
  | Array of type_

let rec string_of_type = function
  | Byte -> "byte"
  | Short -> "short"
  | Char -> "char"
  | Int -> "int"
  | Long -> "long"
  | Float -> "float"
  | Double -> "double"
  | Boolean -> "boolean"
  | Reference (cn, []) -> checked_name cn
  | Reference (cn, l) ->
      let l = List.map string_of_type l in
      Printf.sprintf "%s<%s>" (checked_name cn) (String.concat ", " l)
  | Array t -> (string_of_type t) ^ "[]"

type expression =
  | Call of expression * string * (expression list)
  | Static_call of string * string * (expression list)
  | Identifier of string
  | String_literal of string
  | Int_literal of int32
  | Boolean_literal of bool
  | Null_literal
  | Anonymous_class of type_ * (expression list) * type_ * string * ((type_ * string) list) * expression
  | New of string * expression list
  | New_array of type_ * expression list
  | Cast of type_ * expression
  | Instance_of of expression * type_
  | Infix of string * expression * expression

let rec string_of_expression ?(parens = false) = function
  | Call (x, y, l) ->
      let l = string_of_expression_list l in
      Printf.sprintf "%s.%s(%s)" (string_of_expression ~parens:true x) (checked_name y) (String.concat ", " l)
  | Static_call (x, y, l) ->
      let l = List.map string_of_expression l in
      Printf.sprintf "%s.%s(%s)" (checked_name x) (checked_name y) (String.concat ", " l)
  | Identifier i -> i
  | String_literal s -> Printf.sprintf "%S" s
  | Int_literal i -> Printf.sprintf "%ld" i
  | Boolean_literal false -> "false"
  | Boolean_literal true -> "true"
  | Null_literal -> "null"
  | Anonymous_class (ct, p, rt, mn, mp, e) ->
      let mp =
        List.map
          (fun (t, n) ->
            Printf.sprintf "final %s %s"
              (string_of_type t)
              n)
          mp in
      Printf.sprintf "new %s(%s) { public %s %s(%s) { return %s; } }"
        (string_of_type ct)
        (String.concat ", " (List.map string_of_expression p))
        (string_of_type rt)
        mn
        (String.concat ", " mp)
        (string_of_expression e)
  | New (cn, l) -> Printf.sprintf "new %s(%s)" (checked_name cn) (String.concat ", " (List.map string_of_expression l))
  | New_array (t, l) -> Printf.sprintf "new %s[] { %s }" (string_of_type t) (String.concat ", " (List.map string_of_expression l))
  | Cast (t, e) ->
      if parens then
        Printf.sprintf "((%s) %s)" (string_of_type t) (string_of_expression e)
      else
        Printf.sprintf "(%s) %s" (string_of_type t) (string_of_expression e)
  | Instance_of (e, t) -> Printf.sprintf "%s instanceof %s" (string_of_expression e) (string_of_type t)
  | Infix (op, e1, e2) ->
      Printf.sprintf "(%s) %s (%s)"
        (string_of_expression e1)
        op
        (string_of_expression e2)
and string_of_expression_list l = List.map (fun x -> string_of_expression x) l

type statement =
  | Expression of expression
  | Variable_declaration of type_ * string * expression
  | Return of expression option
  | Super_constructor of expression list
  | Assign of string * expression
  | If of expression * statement
  | If_else of expression * statement * statement
  | Switch of expression * ((int32 * statement) list) * (statement option)
  | Try_catch of block * ((string * string * block) list)
  | Throw of expression
  | Block of block
  | Synchronized_block of expression * block
and block = statement list

let rec dump_statement ?(indent = 0) buff s =
  let add_string s = Buffer.add_string buff s in
  let add_char c = Buffer.add_char buff c in
  let old_indent = indent in
  let indent () = for _i = 1 to 4 * indent do add_char ' ' done in
  match s with
  | Expression e ->
      indent ();
      add_string ((string_of_expression e) ^ ";");
      add_char '\n'
  | Variable_declaration (t, id, e) ->
      indent ();
      add_string (Printf.sprintf "final %s %s = %s;"
                    (string_of_type t)
                    id
                    (string_of_expression e));
      add_char '\n'
  | Return (Some e) ->
      indent ();
      add_string (Printf.sprintf "return %s;" (string_of_expression e));
      add_char '\n'
  | Return None ->
      indent ();
      add_string "return;";
      add_char '\n'
  | Super_constructor l ->
      indent ();
      add_string (Printf.sprintf "super(%s);"
                    (String.concat ", " (List.map string_of_expression l)));
      add_char '\n'
  | Assign (id, e) ->
      indent ();
      add_string (Printf.sprintf "%s = %s;" id (string_of_expression e));
      add_char '\n'
  | If (cond, ifb) ->
      indent ();
      add_string (Printf.sprintf "if (%s) {\n" (string_of_expression cond));
      begin match ifb with
      | Block b -> dump_block ~indent:(succ old_indent) buff b
      | s -> dump_statement ~indent:(succ old_indent) buff s
      end;
      indent ();
      add_string "}\n"
  | If_else (cond, ifb, elseb) ->
      indent ();
      add_string (Printf.sprintf "if (%s) {\n" (string_of_expression cond));
      begin match ifb with
      | Block b -> dump_block ~indent:(succ old_indent) buff b
      | s -> dump_statement ~indent:(succ old_indent) buff s
      end;
      indent ();
      add_string "} else {\n";
      begin match elseb with
      | Block b -> dump_block ~indent:(succ old_indent) buff b
      | s -> dump_statement ~indent:(succ old_indent) buff s
      end;
      indent ();
      add_string "}\n"
  | Switch (e, cases, default) ->
      indent ();
      add_string (Printf.sprintf "switch (%s) {\n" (string_of_expression e));
      List.iter
        (fun (v, s) ->
          indent ();
          add_string (Printf.sprintf "case %ld:\n" v);
          (* do not treat block as in if/else because of possible local
             variable declarations *)
          dump_statement ~indent:(succ old_indent) buff s)
        cases;
      begin match default with
      | Some s ->
          indent ();
          add_string "default:\n";
          dump_statement ~indent:(succ old_indent) buff s
      | None -> ()
      end;
      indent ();
      add_string "}\n"
  | Try_catch (body, handlers) ->
      indent ();
      add_string "try {\n";
      dump_block ~indent:(succ old_indent) buff body;
      List.iter
        (fun (exn, id, h) ->
          indent ();
          add_string (Printf.sprintf"} catch (final %s %s) {\n" exn id);
          dump_block ~indent:(succ old_indent) buff h)
        handlers;
      indent ();
      add_string "}\n"
  | Throw e ->
      indent ();
      add_string (Printf.sprintf "throw %s;\n" (string_of_expression e))
  | Block b ->
      indent ();
      add_string "{\n";
      dump_block ~indent:(succ old_indent) buff b;
      indent ();
      add_string "}\n"
  | Synchronized_block (e, b) ->
      indent ();
      add_string "synchronized(";
      add_string (string_of_expression e);
      add_string ") {\n";
      dump_block ~indent:(succ old_indent) buff b;
      indent ();
      add_string "}\n"
and dump_block ?(indent = 0) buff b =
  List.iter
    (fun s ->
      dump_statement ~indent buff s)
    b

type method_ = {
    meth_javadoc : string list;
    meth_annotations : string list;
    meth_modifiers : modifier list;
    meth_generics : string list;
    meth_return_type : type_ option;
    meth_name : string;
    meth_parameters : (type_ * string) list;
    meth_throws : string list;
    meth_body : block;
  }

let dump_method ?(indent = 0) buff m =
  let add_string s = Buffer.add_string buff s in
  let add_char c = Buffer.add_char buff c in
  let old_indent = indent in
  let indent () = for _i = 1 to 4 * indent do add_char ' ' done in
  if m.meth_javadoc <> [] then begin
    indent ();
    add_string "/**\n";
    List.iter
      (fun l ->
        indent (); add_string " * ";
        add_string l;
        add_char '\n')
      m.meth_javadoc;
    indent (); add_string " */\n"
  end;
  List.iter
    (fun a ->
      indent (); add_string a; add_char '\n')
    m.meth_annotations;
  indent (); add_string (String.concat " " (List.map string_of_modifier m.meth_modifiers));
  if m.meth_modifiers <> [] then add_char ' ';
  if m.meth_generics <> [] then begin
    add_string (Printf.sprintf "<%s> "
                  (String.concat ", " m.meth_generics))
  end;
  begin match m.meth_return_type with
  | Some x -> add_string (string_of_type x)
  | None -> add_string "void"
  end;
  if m.meth_name <> "<init>" then begin
    add_char ' ';
    add_string (checked_name m.meth_name);
  end;
  add_char '(';
  let params =
    List.map
      (fun (t, n) ->
        Printf.sprintf "final %s %s"
          (string_of_type t)
          n)
      m.meth_parameters in
  add_string (String.concat ", " params);
  add_char ')';
  add_char ' ';
  if m.meth_throws <> [] then begin
    add_string "throws ";
    add_string (String.concat ", " m.meth_throws);
    add_char ' '
  end;
  if List.mem Abstract m.meth_modifiers then begin
    add_char ';';
  end else begin
    add_char '{';
    add_char '\n';
    dump_block ~indent:(succ old_indent) buff m.meth_body;
    indent (); add_char '}';
  end;
  add_char '\n';
  add_char '\n'

type class_ = {
    class_package : string option;
    class_imports : string list;
    class_modifiers : modifier list;
    class_name : string;
    class_extends : string option;
    class_fields : ((modifier list) * type_ * string * (expression option)) list;
    class_static_block : block option;
    class_methods : method_ list;
    class_inner_classes : class_or_enum list;
  }
and enum = {
    enum_modifiers : modifier list;
    enum_name : string;
    enum_values : string list;
  }
and interface = {
    interf_modifiers : modifier list;
    interf_name : string;
    interf_methods : (type_ * string * (type_ list)) list;
  }
and class_or_enum =
  | Full_class of class_
  | Enum of enum
  | Interface of interface

let dump_enum ?(indent = 0) buff e =
  let add_string s = Buffer.add_string buff s in
  let add_char c = Buffer.add_char buff c in
  let indent () = for _i = 1 to 4 * indent do add_char ' ' done in
  indent ();
  add_string (Printf.sprintf "%s enum %s { %s }\n\n"
                (String.concat " " (List.map string_of_modifier e.enum_modifiers))
                (checked_name e.enum_name)
                (String.concat ", " e.enum_values))

let dump_interface ?(indent = 0) buff i =
  let add_string s = Buffer.add_string buff s in
  let add_char c = Buffer.add_char buff c in
  let indent () = for _i = 1 to 4 * indent do add_char ' ' done in
  indent ();
  add_string (Printf.sprintf "%s interface %s {\n\n"
                (String.concat " " (List.map string_of_modifier i.interf_modifiers))
                (checked_name i.interf_name));
  List.iter
    (fun (t, n, l) ->
      indent ();
      for _i = 1 to 4 do add_char ' ' done;
      let l =
        List.mapi
          (fun i e -> Printf.sprintf "final %s p%d" (string_of_type e) i)
          l in
      add_string (Printf.sprintf "%s %s(%s);\n\n"
                    (string_of_type t)
                    n
                    (String.concat ", " l)))
    i.interf_methods;
  indent ();
  add_string "}\n\n"

let rec dump_class ?(indent = 0) buff c =
  let add_string s = Buffer.add_string buff s in
  let add_char c = Buffer.add_char buff c in
  let old_indent = indent in
  let indent () = for _i = 1 to 4 * indent do add_char ' ' done in
  begin match c.class_package with
  | Some pack -> add_string (Printf.sprintf "package %s;\n\n" pack)
  | None -> ()
  end;
  List.iter
    (fun i ->
      add_string (Printf.sprintf "import %s;\n" i))
    c.class_imports;
  add_char '\n';
  indent (); add_string (String.concat " " (List.map string_of_modifier c.class_modifiers));
  if c.class_modifiers <> [] then add_char ' ';
  add_string "class ";
  add_string (checked_name c.class_name);
  begin match c.class_extends with
  | Some x -> add_string " extends "; add_string x
  | None -> ()
  end;
  add_string " {\n\n";
  List.iter
    (fun (m, t, n, i) ->
      indent ();
      for _i = 1 to 4 do add_char ' ' done;
      match i with
      | Some e ->
          add_string (Printf.sprintf "%s %s %s = %s;\n\n"
                        (String.concat " " (List.map string_of_modifier m))
                        (string_of_type t)
                        n
                        (string_of_expression e))
      | None ->
          add_string (Printf.sprintf "%s %s %s;\n\n"
                        (String.concat " " (List.map string_of_modifier m))
                        (string_of_type t)
                        n))
    c.class_fields;
  begin match c.class_static_block with
  | Some b ->
      indent ();
      add_string "static {\n";
      dump_block ~indent:(old_indent + 2) buff b;
      indent ();
      add_string "}\n\n"
  | None -> ()
  end;
  List.iter
    (fun m ->
      dump_method ~indent:(succ old_indent) buff m)
    c.class_methods;
  List.iter
    (function
      | Full_class ic ->
          dump_class ~indent:(succ old_indent) buff ic
      | Enum e ->
          dump_enum ~indent:(succ old_indent) buff e
      | Interface i ->
          dump_interface ~indent:(succ old_indent) buff i)
    c.class_inner_classes;
  indent (); add_string "}\n\n"

let rec contains_cast_block l =
  List.exists contains_cast_statement l
and contains_cast_statement = function
  | Expression e -> contains_cast_expression e
  | Variable_declaration (_, _, e) -> contains_cast_expression e
  | Return None -> false
  | Return (Some e) -> contains_cast_expression e
  | Super_constructor (el) -> contains_cast_expression_list el
  | Assign (_, e) -> contains_cast_expression e
  | If (e, s) ->
      (contains_cast_expression e)
    || (contains_cast_statement s)
  | If_else (e, s1, s2) ->
      (contains_cast_expression e)
    || (contains_cast_statement s1)
    || (contains_cast_statement s2)
  | Switch (e, l, d) ->
      (contains_cast_expression e)
    || (List.exists (fun (_, s) -> contains_cast_statement s) l)
    || (match d with Some s -> contains_cast_statement s | None -> false)
  | Try_catch (b, l) ->
      (contains_cast_block b)
    || (List.exists (fun (_, _, b) -> contains_cast_block b) l)
  | Throw e -> (contains_cast_expression e)
  | Block b -> contains_cast_statement_list b
  | Synchronized_block (e, b) ->
      (contains_cast_expression e)
    || (contains_cast_statement_list b)
and contains_cast_statement_list l = List.exists contains_cast_statement l
and contains_cast_expression = function
  | Call (e, _ , el) -> (contains_cast_expression e) || (contains_cast_expression_list el)
  | Static_call (_, _, el) -> contains_cast_expression_list el
  | Identifier _ -> false
  | String_literal _ -> false
  | Int_literal _ -> false
  | Boolean_literal _ -> false
  | Null_literal -> false
  | Anonymous_class (_, _, _, _, _, e) -> contains_cast_expression e
  | New (_, el) -> contains_cast_expression_list el
  | New_array (_, el) -> contains_cast_expression_list el
  | Cast _ -> true
  | Instance_of (e, _) -> contains_cast_expression e
  | Infix (_, e1, e2) -> (contains_cast_expression e1) || (contains_cast_expression e2)
and contains_cast_expression_list l =
  List.exists contains_cast_expression l

let unchecked = "@SuppressWarnings(\"unchecked\")"

let rec add_unchecked_class_or_enum x =
  match x with
  | Full_class cl ->
      Full_class (add_unchecked_class cl)
  | Enum _ | Interface _ ->
      x
and add_unchecked_class cl =
  { cl with class_methods = List.map add_unchecked_method cl.class_methods;
            class_inner_classes = List.map add_unchecked_class_or_enum cl.class_inner_classes; }
and add_unchecked_method m =
  if contains_cast_block m.meth_body then begin
    let already = List.mem unchecked m.meth_annotations in
    if not already then
      { m with meth_annotations = unchecked :: m.meth_annotations; }
    else
      m
  end else
    m

let type_Value = Reference ("Value", [])

let type_Object = Reference ("Object", [])

let type_String = Reference ("String", [])

let constructor ?(javadoc = []) ?(annotations = []) modifiers name ?(parameters = []) body =
  {  meth_javadoc = javadoc;
     meth_annotations = annotations;
     meth_modifiers = modifiers;
     meth_generics = [];
     meth_return_type = Some (Reference (name, []));
     meth_name = "<init>";
     meth_parameters = parameters;
     meth_throws = [];
     meth_body = body; }

let method_ ?(javadoc = []) ?(annotations = []) ?(generics = []) modifiers ?(return = None) name ?(parameters = []) ?(throws = []) body =
  {  meth_javadoc = javadoc;
     meth_annotations = annotations;
     meth_modifiers = modifiers;
     meth_generics = generics;
     meth_return_type = return;
     meth_name = name;
     meth_parameters = parameters;
     meth_throws = throws;
     meth_body = body; }

let class_ modifiers name ?(extends = None) ?(fields = []) ?(static = None) ?(methods = []) ?(inner = []) () =
  { class_package = None;
    class_imports = [];
    class_modifiers = modifiers;
    class_name = name;
    class_extends = extends;
    class_fields = fields;
    class_static_block = static;
    class_methods = methods;
    class_inner_classes = inner; }

let return e =
  Return (Some e)

let and_list l =
  match l with
  | [ x ] -> x
  | hd :: tl ->
      List.fold_left
        (fun acc elem ->
          Infix ("&&", acc, elem))
        hd
        tl
  | [] -> invalid_arg "JavaAST.and_list"

let make_equals_body name param stats =
  let cond = Instance_of (Identifier "obj", Reference (name, [])) in
  let t = Reference (name, []) in
  let if_branch =
    Variable_declaration (t, "that", Cast (t, Identifier param)) :: stats in
  let if_branch = Block if_branch in
  let else_branch = Return (Some (Identifier "false")) in
  [If_else (cond, if_branch, else_branch)]

let make_equals_body_expr_list name param expr_list =
  make_equals_body name param [return (and_list expr_list)]
