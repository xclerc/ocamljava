#!/bin/sh

#
# This file is part of OCaml-Java compiler.
# Copyright (C) 2007-2014 Xavier Clerc.
# Original file (build/partial-install.sh in the OCaml source
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

. config/config.sh

not_installed=$PWD/_build/not_installed

rm -f "$not_installed"
touch "$not_installed"

wontinstall() {
  echo "$1" >> "$not_installed"
  echo "  don't install $1"
}

installbin() {
  if [ -f "$1" ]; then
    echo "  install binary $2"
    cp -f "$1" "$2"
    [ -x "$2" ] || chmod +x "$2"
  else
    wontinstall "$1"
  fi
}

installbestbin() {
  if [ -f "$1" ]; then
    echo "  install binary $3 (with `basename $1`)"
    cp -f "$1" "$3"
  else
    if [ -f "$2" ]; then
      echo "  install binary $3 (with `basename $2`)"
      cp -f "$2" "$3"
    else
      echo "None of $1, $2 exists"
      exit 3
    fi
  fi
  [ -x "$3" ] || chmod +x "$3"
}

installlib() {
  if [ -f "$1" ]; then
    dest="$2/`basename $1`"
    echo "  install library $dest"
    cp -f "$1" "$2"
  else
    wontinstall "$1"
  fi
}

installdir() {
  args=""
  while [ $# -gt 1 ]; do
    if [ -f "$1" ]; then
      args="$args $1"
    else
      wontinstall "$1"
    fi
    shift
  done
  last="$1"
  for file in $args; do
    echo "  install $last/`basename $file`"
    cp -f "$file" "$last"
  done
}

installlibdir() {
  args=""
  while [ $# -gt 1 ]; do
    args="$args $1"
    shift
  done
  last="$1"
  for file in $args; do
    installlib "$file" "$last"
  done
}

mkdir -p $BINDIR
mkdir -p $LIBDIR
mkdir -p $LIBDIR/camlp4

cd _build

echo "Installing camlp4..."
installbin camlp4/camlp4prof.jar $BINDIR/camlp4prof.jar
installbin camlp4/mkcamlp4.jar $BINDIR/mkcamlp4.jar
installbin camlp4/camlp4.jar $BINDIR/camlp4.jar
installbin camlp4/camlp4boot.jar $BINDIR/camlp4boot.jar
installbin camlp4/camlp4o.jar $BINDIR/camlp4o.jar
installbin camlp4/camlp4of.jar $BINDIR/camlp4of.jar
installbin camlp4/camlp4oof.jar $BINDIR/camlp4oof.jar
installbin camlp4/camlp4orf.jar $BINDIR/camlp4orf.jar
installbin camlp4/camlp4r.jar $BINDIR/camlp4r.jar
installbin camlp4/camlp4rf.jar $BINDIR/camlp4rf.jar

if test -d camlp4; then
  cd camlp4
  CAMLP4DIR=$LIBDIR/camlp4
  for dir in Camlp4Parsers Camlp4Printers Camlp4Filters Camlp4Top; do
    echo "Installing $dir..."
    mkdir -p $CAMLP4DIR/$dir
    installdir     \
      $dir/*.cmj*   \
      $dir/*.jo    \
      $CAMLP4DIR/$dir
  done
  installdir \
    camlp4lib.cmja Camlp4.cmi \
    camlp4fulllib.cmja \
    Camlp4Bin.cm[ij] Camlp4Bin.jo Camlp4Top.cmi \
    Camlp4_config.cmi camlp4prof.cm[ij] camlp4prof.jo Camlp4_import.cmi \
    $CAMLP4DIR
  installlibdir camlp4lib.jar camlp4fulllib.jar $CAMLP4DIR
  cd ..
fi
