# Generated automatically from Makefile.in by configure.
# Makefile for library files used by GNU fileutils.
# Do not use this makefile directly, but only from `../Makefile'.
# Copyright (C) 1990, 1991, 1992 Free Software Foundation, Inc.

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

srcdir = .


SOURCES = argmatch.c backupfile.c basename.c dirname.c eaccess.c \
error.c filemode.c fsusage.c getopt.c getopt1.c \
getversion.c idcache.c isdir.c makepath.c \
modechange.c mountlist.c savedir.c \
stripslash.c xgetcwd.c xmalloc.c xstrdup.c yesno.c \
getdate.y posixtm.y \
fileblocks.c fnmatch.c ftruncate.c mkdir.c mktime.c rename.c stpcpy.c \
strdup.c strstr.c

OBJECTS = argmatch.o backupfile.o basename.o dirname.o eaccess.o \
error.o filemode.o getopt.o getopt1.o \
getversion.o idcache.o isdir.o makepath.o \
modechange.o savedir.o \
stripslash.o xgetcwd.o xmalloc.o xstrdup.o yesno.o \
getdate.o posixtm.o \
 fsusage.o mountlist.o stpcpy.o

DISTFILES = Makefile.in backupfile.h getopt.h modechange.h \
fnmatch.h fsusage.h mountlist.h system.h $(SOURCES)

all: libfu.a
.PHONY: all

install: all
.PHONY: install

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

mostlyclean:
	rm -f *.a *.o
.PHONY: mostlyclean

clean: mostlyclean
	rm -f tags TAGS Makefile *.tab.c getdate.c *posixtm.c
.PHONY: clean

realclean: clean
.PHONY: realclean

dist:
	ln $(DISTFILES) ../`cat ../.fname`/lib
.PHONY: dist

libfu.a: $(OBJECTS)
	rm -f $@
	$(AR) cr $@ $(OBJECTS)
	-$(RANLIB) $@

getdate.c: getdate.y
	@echo expect 9 shift/reduce conflicts
	-bison -o getdate.c getdate.y || yacc getdate.y
	test ! -f y.tab.c || mv y.tab.c getdate.c

# Make the rename atomic, in case sed is interrupted and later rerun.
posixtm.c: posixtm.y
	-bison -o posixtm.tab.c posixtm.y || yacc posixtm.y
	test ! -f y.tab.c || mv y.tab.c posixtm.tab.c
	sed -e 's/yy/zz/g' posixtm.tab.c > tposixtm.c
	mv tposixtm.c posixtm.c
	rm -f posixtm.tab.c

backupfile.o getversion.o: backupfile.h
fnmatch.o: fnmatch.h
fsusage.o: fsusage.h
getopt1.o: getopt.h
modechange.o: modechange.h
mountlist.o: mountlist.h
