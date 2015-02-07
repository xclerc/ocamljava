#
# This file is part of OCaml-Java library.
# Copyright (C) 2007-2015 Xavier Clerc.
#
# OCaml-Java library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# OCaml-Java library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

include ../../Makefile.config

LIBRARY_NAME=javalib
OCAMLBUILD=$(PREFIX)/bin/ocamlbuild -classic-display -no-links

default:
	@echo "available targets:"
	@echo "  clean     deletes all produced files"
	@echo "  build     compiles library and generates documentation"
	@echo "  install   copies library files"

clean:
	$(OCAMLBUILD) -clean
	rm -f $(LIBRARY_NAME).mllib $(LIBRARY_NAME).odocl

build:
	$(OCAMLBUILD) $(LIBRARY_NAME).cmja
	$(OCAMLBUILD) $(LIBRARY_NAME).docdir/index.html

install:
	cp _build/$(LIBRARY_NAME).cm* _build/$(LIBRARY_NAME).j* $(PREFIX)/lib/ocaml
	cp _build/src/*.cm* _build/src/*.j* $(PREFIX)/lib/ocaml

.PHONY: default clean build install
