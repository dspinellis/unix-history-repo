#Copyright (C) 1989, 1990 Free Software Foundation, Inc.
#     Written by James Clark (jjc@jclark.uucp)
#
#This file is part of groff.
#
#groff is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 1, or (at your option) any later
#version.
#
#groff is distributed in the hope that it will be useful, but WITHOUT ANY
#WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.
#
#You should have received a copy of the GNU General Public License along
#with groff; see the file LICENSE.  If not, write to the Free Software
#Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

BINDIR=/usr/local/bin
CC=g++
CFLAGS=-g -Wall
INCLUDES=-I../lib
DEFINES=
SOURCES=addftinfo.c guess.c guess.h
OBJECTS=addftinfo.o guess.o
ETAGS=etags
ETAGSFLAGS=-p

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: addftinfo soelim

addftinfo: $(OBJECTS) ../lib/libgroff.a
	  $(CC) $(LDFLAGS) -o $@ $(OBJECTS) ../lib/libgroff.a $(LIBS)

addftinfo.o: guess.h ../lib/lib.h ../lib/errarg.h ../lib/error.h \
	../lib/stringclass.h

guess.o: guess.h

soelim: soelim.o ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ soelim.o ../lib/libgroff.a $(LIBS)

soelim.o: ../lib/lib.h ../lib/errarg.h ../lib/error.h \
	../lib/stringclass.h

TAGS: $(SOURCES)
	$(ETAGS) $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o soelim addftinfo core

distclean: clean
	-rm -f TAGS

realclean: distclean

install.bin: all
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp addftinfo $(BINDIR)/addftinfo
	cp soelim $(BINDIR)/gsoelim

install.nobin:
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp grog.sh $(BINDIR)/grog
	chmod +x $(BINDIR)/grog

install: install.bin install.nobin
