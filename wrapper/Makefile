#
# This file is part of OCaml-Java wrapper.
# Copyright (C) 2007-2014 Xavier Clerc.
#
# OCaml-Java wrapper is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# OCaml-Java wrapper is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

include ../Makefile.config

MAIN_MODULE=ocamlwrap
OCAMLBUILD=$(PREFIX)/bin/ocamlbuild -classic-display -no-links

default:
	@echo "available targets:"
	@echo "  clean     deletes all produced files"
	@echo "  build     compiles all versions (bytecode, native, jar)"
	@echo "  install   copies executable files"

clean:
	$(OCAMLBUILD) -clean

build:
	$(OCAMLBUILD) $(MAIN_MODULE).byte
	$(OCAMLBUILD) $(MAIN_MODULE).native
	$(OCAMLBUILD) $(MAIN_MODULE).jar

install:
	cp _build/src/$(MAIN_MODULE).byte $(PREFIX)/bin/$(MAIN_MODULE)
	cp _build/src/$(MAIN_MODULE).native $(PREFIX)/bin/$(MAIN_MODULE).opt
	cp _build/src/$(MAIN_MODULE).jar $(PREFIX)/bin/$(MAIN_MODULE).jar

.PHONY: default clean build install
