## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2019-2023 Michal Herko <michal.herko@disroot.org>
##
## This file is part of Guile-Web-Driver.
##
## Guile-Web-Driver is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License version 3 as published by
## the Free Software Foundation.
##
## Guile-Web-Driver is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-Web-Driver.  If not, see <http://www.gnu.org/licenses/>.

ACLOCAL_AMFLAGS = -I build-aux

SUBDIRS = build-aux web

EXTRA_DIST = \
	.dir-locals.el

gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    2bea8c70..HEAD > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

dist-hook: gen-ChangeLog
.PHONY: gen-ChangeLog

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES = \
	$(GOBJECTS)

# Makefile.am ends here
