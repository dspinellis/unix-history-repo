# Generated automatically from Makefile.in by configure.
# Makefile for GNU SED, a batch editor.
# Copyright (C) 1987-1991 Free Software Foundation, Inc.
# 
# This file is part of GNU SED.
# 
# GNU SED is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# GNU SED is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU SED; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

SHELL = /bin/sh

#### Start of system configuration section. ####

srcdir = .


CC = gcc -O
INSTALL = install -c

# Things you might add to DEFS:
# -DSTDC_HEADERS	If you have ANSI C headers and libraries.
# -DUSG			If you have System V/ANSI C string
#			and memory functions and headers.
# -DCHAR_UNSIGNED	If type `char' is unsigned.
# -DNO_VFPRINTF		If you lack vprintf function (but have _doprnt).

DEFS =
LIBS = 

CFLAGS = -I$(srcdir) $(DEFS)
LDFLAGS =

prefix = /usr/local

# Where to install the executable.
bindir = $(prefix)/gnubin

#### End of system configuration section. ####

OBJS = sed.o utils.o regex.o getopt.o getopt1.o
SRCS = sed.c utils.c regex.c getopt.c getopt1.c
DISTFILES = COPYING ChangeLog README Makefile.in configure \
regex.h getopt.h $(SRCS)

all:	sed

sed:	$(OBJS)
	$(CC) -o $@ $(LDFLAGS) $(OBJS) $(LIBS)

sed.o regex.o: regex.h
sed.o getopt1.o: getopt.h

install:	all
	$(INSTALL) sed $(bindir)

TAGS:	$(SRCS)
	etags $(SRCS)

clean:
	rm -f sed $(OBJS) core

distclean: clean
	rm -f TAGS Makefile config.status

realclean: distclean

dist:	$(DISTFILES)
	echo sed-`sed -e '/version_string/!d' -e 's/[^0-9.]*\([0-9.]*\).*/\1/' -e q sed.c` > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(DISTFILES) `cat .fname`
	tar chZf `cat .fname`.tar.Z `cat .fname`
	rm -rf `cat .fname` .fname
