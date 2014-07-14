#!/bin/sh

#
# This file is part of OCaml-Java compiler.
# Copyright (C) 2007-2014 Xavier Clerc.
# Original file (build/camlp4-targets.sh in the OCaml source
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

CAMLP4_COMMON="\
  camlp4/Camlp4/Camlp4Ast.partial.ml \
  camlp4/boot/camlp4boot.byte"
CAMLP4_BYTE="$CAMLP4_COMMON \
  camlp4/Camlp4.cmo \
  camlp4/Camlp4Top.cmo \
  camlp4/camlp4prof.byte$EXE \
  camlp4/mkcamlp4.byte$EXE \
  camlp4/camlp4.byte$EXE \
  camlp4/camlp4fulllib.cma"
CAMLP4_NATIVE="$CAMLP4_COMMON \
  camlp4/Camlp4.cmx \
  camlp4/Camlp4Top.cmx \
  camlp4/camlp4prof.native$EXE \
  camlp4/mkcamlp4.native$EXE \
  camlp4/camlp4.native$EXE \
  camlp4/camlp4fulllib.cmxa"
CAMLP4_JAVA="$CAMLP4_COMMON \
  camlp4/Camlp4.cmj \
  camlp4/Camlp4Top.cmj \
  camlp4/camlp4prof.jar \
  camlp4/mkcamlp4.jar \
  camlp4/camlp4.jar \
  camlp4/camlp4fulllib.cmja"

for i in camlp4boot camlp4r camlp4rf camlp4o camlp4of camlp4oof camlp4orf; do
  CAMLP4_BYTE="$CAMLP4_BYTE camlp4/$i.byte$EXE camlp4/$i.cma"
  CAMLP4_NATIVE="$CAMLP4_NATIVE camlp4/$i.native$EXE"
  CAMLP4_JAVA="$CAMLP4_JAVA camlp4/$i.jar"
done

cd camlp4
for dir in Camlp4Parsers Camlp4Printers Camlp4Filters; do
  for file in $dir/*.ml; do
    base=camlp4/$dir/`basename $file .ml`
    CAMLP4_BYTE="$CAMLP4_BYTE $base.cmo"
    CAMLP4_NATIVE="$CAMLP4_NATIVE $base.cmx $base.$O"
    CAMLP4_JAVA="$CAMLP4_JAVA $base.cmj $base.jo"
  done
done
cd ..
