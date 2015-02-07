#!/bin/sh

#
# This file is part of OCaml-Java compiler.
# Copyright (C) 2007-2015 Xavier Clerc.
# Original file (build/camlp4-native-only.sh in the OCaml source
# distribution) is Copyright (C) INRIA.
#
# OCaml-Java compiler is free software; you can redistribute it and/or modify
# it under the terms of the Q Public License v1.0 as published by
# Trolltech (with a change to choice of law).
#
# OCaml-Java compiler is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Q Public License for more details.
#
# You should have received a copy of the Q Public License
# along with this program.  If not, see
# <http://opensource.org/licenses/QPL-1.0>.
#

set -e
cd `dirname $0`/..
. build/targets.sh
set -x

# If you modify this list, modify it also in boot.sh
STDLIB_MODULES='Pervasives,Arg,Array,Buffer,Char,Digest,Filename,Format,Hashtbl,Lazy,Lexing,List,Map,Printexc,Printf,Scanf,Set,String,Sys,Parsing,Int32,Int64,Nativeint,Obj,Queue,Sort,Stream,Stack'

./ocamlbuild/ocamlbuild.native -ignore "$STDLIB_MODULES" $@ java_stdlib_mixed_mode $CAMLP4_JAVA
