# Copyright (C) 2011-2012 Roland Winkler <winkler@gnu.org>
#
# This file is part of the Insidious Big Brother Database (aka BBDB),
#
# BBDB is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# BBDB is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with BBDB.  If not, see <http://www.gnu.org/licenses/>.



prefix = /usr/local
datarootdir = ${prefix}/share
infodir = ${datarootdir}/info
srcdir = .
subdir = doc
top_srcdir = ..


INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

RM   = /usr/bin/rm -f
LN_S = ln -s

MAKEINFO = makeinfo
EMACSINFO = emacs -batch -no-site-file -no-init-file -l texinfmt -f batch-texinfo-format

TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
DVIPS = dvips

all: info

info: bbdb.info
dvi: bbdb.dvi
ps: bbdb.ps
pdf: bbdb.pdf
html: bbdb.html

bbdb.info: bbdb.texi
	@if test "x$(MAKEINFO)" = "x" ; then			\
	   $(EMACSINFO) $< ;					\
	 else							\
	   $(MAKEINFO) $< ;					\
	 fi

bbdb.dvi: bbdb.texi
	@if test "x$(TEXI2DVI)" = "x" ; then			\
	   echo "Sorry, no \`texi2dvi' program available." ;	\
	 else							\
	   $(TEXI2DVI) $< ;					\
	 fi

bbdb.ps: bbdb.dvi
	$(DVIPS) -o $@ $<

bbdb.pdf: bbdb.texi
	@if test "x$(TEXI2PDF)" = "x" ; then			\
	   echo "Sorry, no \`texi2pdf' program available." ;	\
	 else							\
	   $(TEXI2PDF) -p $< ;					\
	 fi

bbdb.html: bbdb.texi
	@if test "x$(MAKEINFO)" = "x" ; then			\
	   echo "Sorry, no \`makeinfo' program available." ;	\
	 else							\
	  $(MAKEINFO) --html -o $@ $< ;				\
	 fi

install-info: bbdb.info
	$(INSTALL) -d -m 0755 "$(infodir)/"
	$(INSTALL_DATA) bbdb.info "$(infodir)/"

uninstall:
	$(RM) "$(infodir)/"bbdb.info

.PHONY: mostlyclean clean distclean maintainer-clean infoclean extraclean

## Temp files.
mostlyclean:
	$(RM) *.aux *.log *.toc *.cp *.cps *.fn *.fns *.ky *.kys \
	  *.op *.ops *.pg *.pgs *.tp *.tps *.vr *.vrs

## Products not in the release tarfiles.
clean: mostlyclean
	$(RM) bbdb.dvi bbdb.pdf bbdb.ps
	rm -rf bbdb.html/

distclean: clean

infoclean:
	$(RM) bbdb.info

maintainer-clean: distclean infoclean

extraclean: maintainer-clean
	-$(RM) *~ \#*
