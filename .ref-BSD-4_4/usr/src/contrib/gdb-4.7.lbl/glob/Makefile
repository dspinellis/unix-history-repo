# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = sysdep.h
host_alias = hp300bsd
host_cpu = m68k
host_vendor = hp
host_os = bsd
target_alias = hp300bsd
target_cpu = m68k
target_vendor = hp
target_os = bsd
ALL=all.internal
#
# Makefile
#   Copyright (C) 1990, 1991, 1992 Free Software Foundation
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
#

# Seriously hacked by K. Richard Pixley <rich@cygnus.com>.

# Makefile for glob source is in -*- text -*- mode.

srcdir = .

prefix = /usr/local

exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib

datadir = $(prefix)/lib

mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
oldincludedir =
docdir = $(datadir)/doc

SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = ar
AR_FLAGS = lq
BISON = bison
MAKEINFO = makeinfo
RANLIB = ranlib

#### Host, target, and site specific Makefile fragments come in here.
###

.c.o:
	$(CC) -c $(CFLAGS) $(H_CFLAGS) -I. $<

CFLAGS = -g
RM = rm -f

#all:	libglob.a
all:
	@echo "Glob doesn't make yet, but readline uses a piece of its src"

.PHONY: check
check:

install:
info install-info clean-info:
libglob.a: glob.o tilde.o
	$(RM) -f $@
	$(AR) $(AR_FLAGS) $@ glob.o tilde.o
	$(RANLIB) libglob.a

glob.o:	glob.c
tilde.o:	tilde.c

install:

tilde-test: tilde.c
	$(CC) $(CFLAGS) -o tilde-test -DTEST tilde.c
clean:
	$(RM) *.o *.a

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

