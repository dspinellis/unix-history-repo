# Generated automatically from Makefile.in by configure.
# Master Makefile for the GNU text utilities.
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

#### Start of system configuration section. ####

srcdir = .


# If you use gcc, you should either run the fixincludes script that
# comes with it or else use gcc with the -traditional option.  Otherwise
# ioctl calls will be compiled incorrectly on some systems.
CC = gcc -O
AR = ar
# Set RANLIB = echo if your system doesn't have or need ranlib.
RANLIB = ranlib 
# Use cp if you don't have install.
INSTALL = /usr/local/bin/install -c
INSTALLDATA = /usr/local/bin/install -c -m 644

# Things you might add to DEFS:
# -DSTDC_HEADERS	If you have ANSI C headers and libraries.
# -DHAVE_UNISTD_H	If you have unistd.h.
# -DUSG			If you have System V/ANSI C string and
#			memory functions and headers.
# -DST_BLKSIZE_MISSING	If your `struct stat' lacks st_blksize.
# -DSIGTYPE=int		If your signal handlers return int, not void.
# -DCHAR_UNSIGNED	If type `char' is unsigned.
# -DSTRERROR_MISSING	If you lack strerror function.
# -DVPRINTF_MISSING	If you lack vprintf function (but have _doprnt).
# -DDOPRNT_MISSING	If you lack _doprnt function.  Also need to define
#			-DVPRINTF_MISSING.

DEFS =  -DSTDC_HEADERS -DHAVE_UNISTD_H

CDEBUG = -g
CFLAGS = $(CDEBUG) -I. -I../lib -I$(srcdir)/lib $(DEFS)
LDFLAGS = -g
LIBS = 

prefix = /usr/src/public/textutils-1.3
# Prefix for each installed program, normally empty or `g'.
binprefix = 
# Prefix for each installed man page, normally empty or `g'.
manprefix = 

# Where to install the executables.
bindir = $(prefix)/bin

# Where to install the manual pages.
mandir = $(prefix)/man/man1
# Extension (not including `.') for the installed manual page filenames.
manext = 1

#### End of system configuration section. ####

MDEFINES = bindir='$(bindir)' mandir='$(mandir)' \
manext='$(manext)' binprefix='$(binprefix)' manprefix='$(manprefix)' \
LIBS='$(LIBS)' LIBPROGS='$(LIBPROGS)' \
AR='$(AR)' RANLIB='$(RANLIB)' INSTALL='$(INSTALL)' \
INSTALLDATA='$(INSTALLDATA)' \
CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' LIBS='$(LIBS)' CC='$(CC)'

DISTFILES = COPYING ChangeLog Makefile.in README configure configure.in

# Redundant stuff for making only selected programs.
PROGS = cat cmp comm csplit cut expand fold head join nl \
paste pr sort split sum tac tail tr unexpand uniq wc

# Subdirectories to run make in for the primary targets.
SUBDIRS = lib src man

all:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
.PHONY: all

$(PROGS): lib/libtu.a
	cd src; $(MAKE) $(MDEFINES) $@

lib/libtu.a:
	cd lib; $(MAKE) $(MDEFINES) all

install:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
.PHONY: install

tags:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
.PHONY: tags

TAGS:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
.PHONY: TAGS

mostlyclean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
.PHONY: mostlyclean

clean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
	rm -f Makefile config.status
.PHONY: clean

realclean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
	rm -f Makefile config.status
.PHONY: realclean

dist:
	echo textutils-`sed -e '/version_string/!d' -e 's/[^0-9.]*\([0-9.]*\).*/\1/' -e q < src/version.c` > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(DISTFILES) `cat .fname`
	for dir in $(SUBDIRS); do mkdir `cat .fname`/$$dir; cd $$dir; $(MAKE) $@; cd ..; done
	tar chZf `cat .fname`.tar.Z `cat .fname`
	rm -rf `cat .fname` .fname
