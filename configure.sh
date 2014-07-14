#!/bin/sh

#
# This file is part of OCaml-Java build.
# Copyright (C) 2007-2014 Xavier Clerc.
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

# prints the passed message
warn() {
    echo "*** warning: $1"
}

# prints the help message, and terminates the script
print_usage() {
    echo "usage: $0 [options]"
    echo 'available options:'
    echo '  -prefix <path>               set installation path'
    echo '  -mode <compatibility|speed>  set runtime/compiler mode'
    echo '  -ocaml-configure <...>       set options to pass to OCaml configure'
    exit 0
}

# checks Ant and Java versions, terminating the script if any is invalid
check_ant_and_java_versions() {
    local ant_stdout
    local ant_exit_code
    ant_stdout=`ant -q -S -f build/build-check-versions.xml 2>&1`
    ant_exit_code=$?
    case $ant_exit_code in
        0) # Ant execution was successful
            ;;
        1) # Ant execution failed
            fail "`echo $ant_stdout | sed -e 's/.*!\(.*\)!.*/\1/'`"
            ;;
        127) # Ant was not executed
            fail "unable to run Ant"
            ;;
    esac
}

# checks dependencies
check_ant_and_java_versions

# default values
prefix=''
mode='compatibility'
ocaml_configure=''

# parses command-line
while [ $# -gt 0 ]
do
    case "$1" in
        -prefix)
            prefix="$2";
            shift
            ;;
        -mode)
            mode="$2";
            shift
            ;;
        -ocaml-configure)
            ocaml_configure="$2";
            shift
            ;;
        -help|--help)
            print_usage
            ;;
        *)
            fail "invalid switch ($1)"
            ;;
    esac
    shift
done

# performs sanity checks over command-line elements
if [ "$prefix" = "" ]; then
    fail 'an installation prefix should be given through -prefix'
fi
case "$prefix" in
    /*) # the prefix value is absolute (leading '/')
        ;;
    *)
        fail 'the prefix should be absolute'
        ;;
esac
case "$mode" in
    compatibility)
        ;;
    speed)
        fail 'the "speed" mode is not currently supported'
        ;;
    *)
        fail 'the mode should be either "compatibility" or "speed"'
        ;;
esac

# outputs configuration summary
echo 'configuration:'
echo "  PREFIX ---------------> $prefix"
echo "  MODE -----------------> $mode"
echo "  OCAML_CONFIGURE ------> $ocaml_configure"

# creates the configuration file
echo "# timestamp: `date`" > Makefile.config
echo 'VERSION="'`cat VERSION`'"' >> Makefile.config
echo 'PREFIX="'$prefix'"' >> Makefile.config
case "$mode" in
    compatibility)
        echo "OCAML_INTS_ARE_63_BIT_LONG=TRUE" >> Makefile.config
        ;;
    speed)
        echo "# OCAML_INTS_ARE_63_BIT_LONG=FALSE" >> Makefile.config
        ;;
esac
echo 'OCAML_CONFIGURE="'$ocaml_configure'"' >> Makefile.config

# checks that JAVA_HOME is set
if [ -z "$JAVA_HOME" ] && [ ! -x "/usr/libexec/java_home" ]; then
    echo ''
    warn '"JAVA_HOME" environment variable should be set'
fi

# informs that configuration is done
echo ''
echo '*** configuration successfully written to "Makefile.config"'
exit 0
