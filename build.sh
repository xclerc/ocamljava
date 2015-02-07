#!/bin/sh

#
# This file is part of OCaml-Java build.
# Copyright (C) 2007-2015 Xavier Clerc.
#
# OCaml-Java build is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# OCaml-Java build is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


# prints the passed message, and terminates the script
fail() {
    echo "*** error: $1"
    exit 1
}

# checks that the configuration file has been generated
if [ ! -f "Makefile.config" ]; then
    fail 'no "Makefile.config" file, please run "configure.sh"'
fi

# checks that JAVA_HOME is set
if [ -z "$JAVA_HOME" ] && [ ! -x "/usr/libexec/java_home" ]; then
    fail '"JAVA_HOME" environment variable should be set'
fi

# creates the "_build" directory, if needed
mkdir -p _build

# copies Makefile elements to the "_build" directory
cp build/Makefile _build
cp Makefile.config _build
cp Makefile.project _build
cp third-party/Makefile.versions _build

# launches the build process
cd _build && make
