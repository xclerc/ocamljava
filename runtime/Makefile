#
# This file is part of OCaml-Java runtime.
# Copyright (C) 2007-2015 Xavier Clerc.
#
# OCaml-Java runtime is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# OCaml-Java runtime is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


include ../Makefile.config

default:
	@echo "no target provided"
	exit 1

build:
	ant build

install:
	cp build/ocamlrun.primitives \
	   build/ocamlrun.jar \
	   build/ocamlrun-labltk.jar \
	   $(PREFIX)/lib/ocaml

.PHONY: default
.PHONY: build
.PHONY: install
