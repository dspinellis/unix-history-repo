# Generated automatically from Makefile.in by configure.
# Makefile for GNU shell utilities documentation.
# Do not use this makefile directly, but only from `../Makefile'.
# Copyright (C) 1991 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

SHELL = /bin/sh

srcdir = ./man


# Files to install on all systems
MANFILES1 = basename.1 date.1 dirname.1 env.1 expr.1 groups.1 \
id.1 logname.1 pathchk.1 printenv.1 printf.1 sleep.1 tee.1 test.1 \
tty.1 whoami.1 yes.1
# Files to maybe install.
MANFILES2 = nice.1 nohup.1 stty.1 uname.1

# Files to install.
MANFILES = $(MANFILES1)  nice.1 nohup.1

DISTFILES = Makefile.in $(MANFILES1) $(MANFILES2)

all:
.PHONY: all

install: all
	for f in $(MANFILES); do \
	if test -f $$f; then src=.; else src=$(srcdir); fi; \
	$(INSTALLDATA) $$src/$$f $(mandir)/$(manprefix)`basename $$f 1`$(manext); \
	done
.PHONY: install

tags:
.PHONY: tags

TAGS:
.PHONY: TAGS

mostlyclean:
.PHONY: mostlyclean

clean: mostlyclean
	rm -f Makefile
.PHONY: clean

realclean: clean
.PHONY: realclean

dist:
	ln $(DISTFILES) ../`cat ../.fname`/man
.PHONY: dist
