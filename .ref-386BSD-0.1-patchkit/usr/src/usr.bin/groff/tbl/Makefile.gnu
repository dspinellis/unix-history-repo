#Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.
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
CFLAGS=-g
INCLUDES=-I../lib
DEFINES=
SOURCES=main.c table.c table.h
MISC=Makefile TODO
ETAGS=etags
ETAGSFLAGS=-p
BINDIR=/usr/local/bin
OBJECTS=main.o table.o

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: tbl

tbl: $(OBJECTS) ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) ../lib/libgroff.a $(LIBS)

TABLE_H=table.h ../lib/cset.h ../lib/cmap.h ../lib/stringclass.h \
        ../lib/errarg.h ../lib/error.h ../lib/lib.h

main.o: $(TABLE_H) 
table.o: $(TABLE_H)

TAGS : $(SOURCES)
	$(ETAGS) $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o core tbl

distclean: clean
	-rm -f TAGS

realclean: distclean

install.bin: tbl
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp tbl $(BINDIR)/gtbl

install.nobin:

install: install.bin install.nobin
