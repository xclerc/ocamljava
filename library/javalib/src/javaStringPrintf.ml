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

let fprintf chan str =
  JavaString.output_string chan str

let printf x y = fprintf x y

let eprintf x y = fprintf x y

let sprintf () str =
  JavaString.to_string str

let bprintf buff str =
  Buffer.add_string buff (JavaString.to_string str)

let ifprintf _dummy _str =
  ()

let kfprintf  x y = fprintf x y

let ikfprintf x y = fprintf x y

let ksprintf  x y = sprintf x y

let kbprintf  x y = bprintf x y
