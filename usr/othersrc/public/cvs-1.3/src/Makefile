# Generated automatically from Makefile.in by configure.
# Makefile for GNU CVS program.
# Do not use this makefile directly, but only from `../Makefile'.
# Copyright (C) 1986, 1988-1990 Free Software Foundation, Inc.

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

# @(#)Makefile.in 1.10 92/03/16

SHELL = /bin/sh

srcdir = .


SOURCES = add.c admin.c checkin.c checkout.c classify.c commit.c \
create_adm.c diff.c entries.c find_names.c hash.c history.c ignore.c \
import.c lock.c log.c logmsg.c main.c myndbm.c rcs.c modules.c \
no_diff.c parseinfo.c patch.c recurse.c release.c remove.c repos.c rtag.c \
status.c tag.c update.c vers_ts.c version.c subr.c
MSOURCES = mkmodules.c

OBJECTS = add.o admin.o checkin.o checkout.o classify.o commit.o \
create_adm.o diff.o entries.o find_names.o hash.o history.o ignore.o \
import.o lock.o log.o logmsg.o main.o myndbm.o rcs.o modules.o \
no_diff.o parseinfo.o patch.o recurse.o release.o remove.o repos.o rtag.o \
status.o tag.o update.o vers_ts.o
MOBJECTS = hash.o mkmodules.o myndbm.o

HEADERS = config.h cvs.h rcs.h hash.h myndbm.h patchlevel.h

DISTFILES = Makefile.in $(HEADERS) $(SOURCES) $(MSOURCES)

PROGS = cvs mkmodules

xxx:
	@cd ..; $(MAKE) all SUBDIRS=src

saber_cvs:
	@cd ..; $(MAKE) saber SUBDIRS=src

lint:
	@cd ..; $(MAKE) lint SUBDIRS=src

all: $(PROGS)
.PHONY: all

install: all
	@for prog in $(PROGS); do echo Installing $$prog in $(bindir); $(INSTALL) $$prog $(bindir); done
.PHONY: install

tags: $(DISTFILES)
	ctags $(DISTFILES)

TAGS: $(DISTFILES)
	etags $(DISTFILES)

ls:
	@echo $(DISTFILES)
.PHONY: ls

clean:
	/bin/rm -f $(PROGS) *.o core
.PHONY: clean

distclean: clean
	rm -f tags TAGS Makefile
.PHONY: distclean

realclean: distclean
.PHONY: realclean

dist:
	ln $(DISTFILES) ../`cat ../.fname`/src
.PHONY: dist

# Linking rules.

$(PROGS): subr.o version.o ../lib/libcvs.a

cvs: $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) subr.o version.o ../lib/libcvs.a $(LIBS)

xlint: $(SOURCES)
	sh -c "lint $(DEFS) $(INCLUDES) $(SOURCES) | grep -v \"possible pointer alignment problem\""

saber: $(SOURCES)
	# load $(CFLAGS) $(SOURCES)
	# load ../lib/libcvs.a $(LIBS)

mkmodules: $(MOBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(MOBJECTS) subr.o version.o ../lib/libcvs.a $(LIBS)

# Compilation rules.

$(OBJECTS) mkmodules.o subr.o: $(HEADERS)
