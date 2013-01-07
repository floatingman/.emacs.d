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



INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
texmf_dir = /usr/share/texmf-site/bbdb

RM   = /usr/bin/rm -f
LN_S = ln -s

install-tex:
	if test "x$(texmf_dir)" != "x"; then		\
	    $(INSTALL) -d -m 0755 "$(texmf_dir)/";	\
	    for tex in *.tex; do			\
	        echo "Install $$tex in $(texmf_dir)/";	\
	        $(INSTALL_DATA) $$tex "$(texmf_dir)/";	\
            done;					\
	fi

uninstall:
	for tex in *.tex; do		\
	    $(RM) "$(texmf_dir)/$$tex";	\
	done;

clean:

distclean: clean

maintainer-clean: distclean
	-$(RM) Makefile

extraclean: maintainer-clean
	-$(RM) -f *~ \#*
