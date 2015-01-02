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


include ./Makefile.versions
include ./Makefile.config

AUGMENTED_PATH=export PATH=$(PREFIX)/bin:`echo $$PATH`

default: \
	prepare \
	step00-extract-third-party-jar-files \
	step01-build-and-install-ocaml \
	step02-build-and-install-camlzip \
	step03-build-and-install-camomile \
	step04-build-and-install-barista \
	step05-build-and-install-runtime \
	step06-build-and-install-byte-and-native-compilers-and-libraries \
	step07-build-and-install-extended-ocamlbuild \
	step08-build-and-install-extended-ocamldoc-byte-and-native \
	step09-build-and-install-ocamljava-libraries \
	step10-build-and-install-barista-with-ocamljava \
	step11-build-and-install-java-toplevel \
	step12-build-and-install-extended-ocamldoc-jar \
	step13-build-and-install-optimizer \
	step14-build-and-install-ocamllex-jar \
	step15-build-and-install-tools \
	step16-build-and-install-ocamljava-jar \
	step17-build-and-install-wrapper \
	step18-build-and-install-ocamlc-jar \
	step19-build-and-install-ocamldebug-jar \
	step20-build-and-install-camlp4 \
	step21-generate-and-copy-documentation

prepare:
	test -d timestamps && rm -fr timestamps || true
	mkdir timestamps

step00-extract-third-party-jar-files:
	touch timestamps/step00-extract-third-party-jar-files
	test -d external-jars && rm -fr external-jars || true
	test -d jacl$(JACL_VERSION_NO_DOTS) && rm -fr jacl$(JACL_VERSION_NO_DOTS) || true
	test -d swank && rm -fr swank || true
	mkdir external-jars
	cp ../third-party/jaclBinary$(JACL_VERSION_NO_DOTS).zip . && \
	unzip jaclBinary$(JACL_VERSION_NO_DOTS).zip && \
	export DIR=jacl$(JACL_VERSION_NO_DOTS)/lib/tcljava$(JACL_VERSION) && \
	cp `ls $$DIR/*.jar | grep -v src` external-jars && \
	cp ../third-party/swank.tar.gz . && \
	tar xfz swank.tar.gz && \
	cp swank/*.jar external-jars && \
	cp ../third-party/servlet-api.jar external-jars

step01-build-and-install-ocaml:
	touch timestamps/step01-build-and-install-ocaml
	test -d ocaml-$(OCAML_VERSION) && rm -fr ocaml-$(OCAML_VERSION) || true
	cp ../third-party/ocaml-$(OCAML_VERSION).tar.gz . && \
	tar xfz ocaml-$(OCAML_VERSION).tar.gz && \
	cd ocaml-$(OCAML_VERSION) && \
	cp ../../third-party/ocaml-$(OCAML_VERSION).patch . && \
	patch -f -p1 < ocaml-$(OCAML_VERSION).patch && \
	./configure `echo $(OCAML_CONFIGURE)` -prefix $(PREFIX) && \
	(grep -q '^#undef ARCH_SIXTYFOUR$$' config/m.h && \
	  echo '*** OCaml-Java requires a 64-bit platform to build' && exit 1 || true) && \
	$(MAKE) world.opt && \
	$(MAKE) install && \
	cd ..
	if [ -d $(PREFIX)/lib/ocaml/external-jars ]; then \
	  rm -fr $(PREFIX)/lib/ocaml/external-jars; \
	fi
	cp -R external-jars $(PREFIX)/lib/ocaml

step02-build-and-install-camlzip:
	touch timestamps/step02-build-and-install-camlzip
	test -d camlzip-$(CAMLZIP_VERSION) && rm -fr camlzip-$(CAMLZIP_VERSION) || true
	$(AUGMENTED_PATH) && \
	cp ../third-party/camlzip-$(CAMLZIP_VERSION).tar.gz . && \
	tar xfz camlzip-$(CAMLZIP_VERSION).tar.gz && \
	cd camlzip-$(CAMLZIP_VERSION) && \
	$(MAKE) all allopt && \
	$(MAKE) install installopt && \
	cd ..

step03-build-and-install-camomile:
	touch timestamps/step03-build-and-install-camomile
	test -d camomile-$(CAMOMILE_VERSION) && rm -fr camomile-$(CAMOMILE_VERSION) || true
	test -f camomile-$(CAMOMILE_VERSION).tar && rm -f camomile-$(CAMOMILE_VERSION).tar || true
	$(AUGMENTED_PATH) && \
	cp ../third-party/camomile-$(CAMOMILE_VERSION).tar.bz2 . && \
	bunzip2 camomile-$(CAMOMILE_VERSION).tar.bz2 && \
	tar xf camomile-$(CAMOMILE_VERSION).tar && \
	cd camomile-$(CAMOMILE_VERSION) && \
	./configure --prefix=$(PREFIX) --datadir=$(PREFIX)/share && \
	$(MAKE) && \
	$(MAKE) install-without-ocamlfind install-data && \
	cd ..

step04-build-and-install-barista:
	touch timestamps/step04-build-and-install-barista
	test -d barista-$(BARISTA_VERSION) && rm -fr barista-$(BARISTA_VERSION) || true
	$(AUGMENTED_PATH) && \
	cp ../third-party/barista-$(BARISTA_VERSION).tar.gz . && \
	tar xfz barista-$(BARISTA_VERSION).tar.gz && \
	cd barista-$(BARISTA_VERSION) && \
	./configure -ocamlfind '' && \
	$(MAKE) all && \
	$(MAKE) install && \
	cd .. \

step05-build-and-install-runtime:
	touch timestamps/step05-build-and-install-runtime
	test -d runtime && rm -fr runtime || true
	cp -R ../runtime .
	cd runtime && \
	$(MAKE) build && \
	$(MAKE) install && \
	cd ..

step06-build-and-install-byte-and-native-compilers-and-libraries:
	touch timestamps/step06-build-and-install-byte-and-native-compilers-and-libraries
	$(AUGMENTED_PATH) && \
	cp -R ../compiler/* ocaml-$(OCAML_VERSION) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-byte-and-native-compilers-and-libraries && \
	$(MAKE) -f Makefile-ocamljava install-byte-and-native-compilers-and-libraries && \
	cd ..

step07-build-and-install-extended-ocamlbuild:
	touch timestamps/step07-build-and-install-extended-ocamlbuild
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-extended-ocamlbuild && \
	$(MAKE) -f Makefile-ocamljava install-extended-ocamlbuild && \
	cd ..

step08-build-and-install-extended-ocamldoc-byte-and-native:
	touch timestamps/step08-build-and-install-extended-ocamldoc-byte-and-native
	mkdir -p $(PREFIX)/lib/ocaml/ocamldoc/bootstrap
	cp ../third-party/bootstrap-$(BOOTSTRAP_VERSION)-dist.zip . && \
	unzip bootstrap-$(BOOTSTRAP_VERSION)-dist.zip && \
	cat bootstrap-$(BOOTSTRAP_VERSION)-dist/css/bootstrap.min.css | \
	sed -e 's|../fonts/||g' > $(PREFIX)/lib/ocaml/ocamldoc/bootstrap/bootstrap.min.css && \
	cp bootstrap-$(BOOTSTRAP_VERSION)-dist/fonts/* $(PREFIX)/lib/ocaml/ocamldoc/bootstrap
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-extended-ocamldoc-byte-and-native && \
	$(MAKE) -f Makefile-ocamljava install-extended-ocamldoc-byte-and-native && \
	cd ..

step09-build-and-install-ocamljava-libraries:
	touch timestamps/step09-build-and-install-ocamljava-libraries
	test -d library && rm -fr library || true
	$(AUGMENTED_PATH) && \
	cp -R ../library . && \
	for lib in javalib concurrent; do \
	  cd library/$$lib && \
	  $(MAKE) build && \
	  $(MAKE) install && \
	  cd ../..; \
	done

step10-build-and-install-barista-with-ocamljava:
	touch timestamps/step10-build-and-install-barista-with-ocamljava
	$(AUGMENTED_PATH) && \
	cd barista-$(BARISTA_VERSION) && \
	$(MAKE) veryclean && \
	$(MAKE) ocamljava && \
	$(MAKE) install-only-ocamljava && \
	cd ..

step11-build-and-install-java-toplevel:
	touch timestamps/step11-build-and-install-java-toplevel
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-java-toplevel && \
	$(MAKE) -f Makefile-ocamljava install-java-toplevel && \
	$(MAKE) -f Makefile-ocamljava script.jar && \
	cd ..

step12-build-and-install-extended-ocamldoc-jar:
	touch timestamps/step12-build-and-install-extended-ocamldoc-jar
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-extended-ocamldoc-jar && \
	$(MAKE) -f Makefile-ocamljava install-extended-ocamldoc-jar && \
	cd ..

step13-build-and-install-optimizer:
	touch timestamps/step13-build-and-install-optimizer
	test -d optimizer && rm -fr optimizer || true
	$(AUGMENTED_PATH) && \
	cp -R ../optimizer . && \
	cd optimizer && \
	$(MAKE) build && \
	$(MAKE) install && \
	cd ..

step14-build-and-install-ocamllex-jar:
	touch timestamps/step14-build-and-install-ocamllex-jar
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-ocamllex-jar && \
	$(MAKE) -f Makefile-ocamljava install-ocamllex-jar && \
	cd ..

step15-build-and-install-tools:
	touch timestamps/step15-build-and-install-tools
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-tools && \
	$(MAKE) -f Makefile-ocamljava install-tools && \
	cd ..

step16-build-and-install-ocamljava-jar:
	touch timestamps/step16-build-and-install-ocamljava-jar
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-ocamljava-jar && \
	$(MAKE) -f Makefile-ocamljava install-ocamljava-jar && \
	cd ..

step17-build-and-install-wrapper:
	touch timestamps/step17-build-and-install-wrapper
	test -d wrapper && rm -fr wrapper || true
	$(AUGMENTED_PATH) && \
	cp -R ../wrapper . && \
	cd wrapper && \
	$(MAKE) build && \
	$(MAKE) install && \
	cd ..

step18-build-and-install-ocamlc-jar:
	touch timestamps/step18-build-and-install-ocamlc-jar
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-ocamlc-jar && \
	$(MAKE) -f Makefile-ocamljava install-ocamlc-jar && \
	cd ..

step19-build-and-install-ocamldebug-jar:
	touch timestamps/step19-build-and-install-ocamldebug-jar
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	rm -f typing/*.cm* && \
	rm -f bytecomp/*.cm* && \
	rm -f toplevel/*.cm* && \
	$(MAKE) -f Makefile-ocamljava prepare-debugger && \
	cd debugger && \
	$(MAKE) -f Makefile-ocamljava all && \
	$(MAKE) -f Makefile-ocamljava install && \
	cd ../..

step20-build-and-install-camlp4:
	touch timestamps/step20-build-and-install-camlp4
	$(AUGMENTED_PATH) && \
	cd ocaml-$(OCAML_VERSION) && \
	$(MAKE) -f Makefile-ocamljava build-camlp4 && \
	$(MAKE) -f Makefile-ocamljava install-camlp4 && \
	cd ..

step21-generate-and-copy-documentation:
	touch timestamps/step21-generate-and-copy-documentation
	for dir in concurrent javalib manual ocjlib stdlib wrappers; do \
	  if [ -d $(PREFIX)/doc/ocamljava/$$dir ]; then \
	    rm -fr $(PREFIX)/doc/ocamljava/$$dir; \
	  fi; \
	done
	mkdir -p $(PREFIX)/doc/ocamljava
	mkdir $(PREFIX)/doc/ocamljava/stdlib
	mkdir $(PREFIX)/doc/ocamljava/ocjlib
	cd ocaml-$(OCAML_VERSION) && \
	./ocamldoc/ocamldoc.opt \
	  -html -html5 -sort \
	  -t 'OCaml standard library' \
	  -d $(PREFIX)/doc/ocamljava/stdlib \
	  -I stdlib \
	  -I otherlibs/bigarray \
	  -I otherlibs/dynlink \
	  -I otherlibs/graph \
	  -I otherlibs/num \
	  -I otherlibs/str \
	  -I otherlibs/unix \
	  stdlib/*.mli \
	  otherlibs/bigarray/bigarray.mli \
	  otherlibs/dynlink/dynlink.mli \
	  otherlibs/graph/graphics.mli \
	  otherlibs/num/*.mli \
	  otherlibs/str/str.mli \
	  otherlibs/unix/unix.mli && \
	cd ..
	cd ocaml-$(OCAML_VERSION) && \
	./ocamldoc/ocamldoc.opt \
	  -html -html5 -sort \
	  -t 'OCaml-Java library' \
	  -java-extensions \
	  -cp ../external-jars/servlet-api.jar \
	  -d $(PREFIX)/doc/ocamljava/ocjlib \
	  -I stdlib \
	  -I otherlibs/bigarray \
	  -I otherlibs/dynlink \
	  -I otherlibs/graph \
	  -I otherlibs/num \
	  -I otherlibs/str \
	  -I otherlibs/unix \
	  `find ../library/javalib/_build/src -type d | xargs -I {} echo -I {} | xargs` \
	  `find ../library/concurrent/_build/src -type d | xargs -I {} echo -I {} | xargs` \
	  stdlib/*.mli \
	  otherlibs/bigarray/bigarray.mli \
	  otherlibs/dynlink/dynlink.mli \
	  otherlibs/graph/graphics.mli \
	  otherlibs/num/*.mli \
	  otherlibs/str/str.mli \
	  otherlibs/unix/unix.mli \
	  `find ../library/javalib/_build/src -type d | xargs -I {} echo {}/*.mli | xargs` \
	  `find ../library/concurrent/_build/src -type d | xargs -I {} echo {}/*.mli | xargs` && \
	cd ..
	cp -R library/concurrent/_build/concurrent.docdir \
	      $(PREFIX)/doc/ocamljava
	mv $(PREFIX)/doc/ocamljava/concurrent.docdir \
	   $(PREFIX)/doc/ocamljava/concurrent
	rm $(PREFIX)/doc/ocamljava/concurrent/html.stamp
	cp -R library/javalib/_build/javalib.docdir \
	      $(PREFIX)/doc/ocamljava
	mv $(PREFIX)/doc/ocamljava/javalib.docdir \
	   $(PREFIX)/doc/ocamljava/javalib
	rm $(PREFIX)/doc/ocamljava/javalib/html.stamp
	cp -R ../doc/manual $(PREFIX)/doc/ocamljava
	cp -R ../doc/*.pdf $(PREFIX)/doc/ocamljava
	cp -R ../runtime/javadoc/wrappers $(PREFIX)/doc/ocamljava

.PHONY: default
.PHONY: prepare
.PHONY: step00-extract-third-party-jar-files
.PHONY: step01-build-and-install-ocaml
.PHONY: step02-build-and-install-camlzip
.PHONY: step03-build-and-install-camomile
.PHONY: step04-build-and-install-barista
.PHONY: step05-build-and-install-runtime
.PHONY: step06-build-and-install-byte-and-native-compilers-and-libraries
.PHONY: step07-build-and-install-extended-ocamlbuild
.PHONY: step08-build-and-install-extended-ocamldoc-byte-and-native
.PHONY: step09-build-and-install-ocamljava-libraries
.PHONY: step10-build-and-install-barista-with-ocamljava
.PHONY: step11-build-and-install-java-toplevel
.PHONY: step12-build-and-install-extended-ocamldoc-jar
.PHONY: step13-build-and-install-optimizer
.PHONY: step14-build-and-install-ocamllex-jar
.PHONY: step15-build-and-install-tools
.PHONY: step16-build-and-install-ocamljava-jar
.PHONY: step17-build-and-install-wrapper
.PHONY: step18-build-and-install-ocamlc-jar
.PHONY: step19-build-and-install-ocamldebug-jar
.PHONY: step20-build-and-install-camlp4
.PHONY: step21-generate-and-copy-documentation
