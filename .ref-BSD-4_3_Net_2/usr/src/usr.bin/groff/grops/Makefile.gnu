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

# define PAGE to be letter if your PostScript printer uses 8.5x11 paper (USA)
# and define it to be A4, if it uses A4 paper (rest of the world)
PAGE=A4
#PAGE=letter
BINDIR=/usr/local/bin
CC=g++
CFLAGS=-g #-DBROKEN_SPOOLER
OLDCC=gcc
OLDCFLAGS=-g
MLIBS=-lm
INCLUDES=-I../driver -I../lib
DEFINES=
LDFLAGS=-g
OBJECTS=ps.o
SOURCES=ps.c
MISC=Makefile devgps
BINDIR=/usr/local/bin
FONTDIR=/usr/local/lib/groff/font
MACRODIR=/usr/local/lib/groff/tmac
ETAGS=etags
ETAGSFLAGS=-p

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: grops psbb devps

grops: $(OBJECTS) ../driver/libdriver.a ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) \
	../driver/libdriver.a ../lib/libgroff.a $(MLIBS)

ps.o: ../driver/printer.h ../driver/driver.h ../lib/font.h \
      ../lib/stringclass.h ../lib/cset.h

psbb: psbb.o
	$(OLDCC) $(LDFLAGS) -o $@ psbb.o

psbb.o: psbb.c
	$(OLDCC) $(OLDCFLAGS) -c psbb.c

install.bin: grops psbb
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp grops psbb $(BINDIR)
	@cd devps; \
	$(MAKE) \
	"FONTDIR=$(FONTDIR)" "PAGE=$(PAGE)" "BINDIR=$(BINDIR)" install.bin

install.nobin:
	-[ -d $(MACRODIR) ] || mkdir $(MACRODIR)
	cp tmac.ps $(MACRODIR)
	@echo Making install.nobin in devps
	@cd devps; \
	$(MAKE) \
	"FONTDIR=$(FONTDIR)" "PAGE=$(PAGE)" "BINDIR=$(BINDIR)" install.nobin

install: install.bin install.nobin


TAGS : $(SOURCES)
	$(ETAGS) $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o psbb core grops

distclean: clean
	-rm -f TAGS

realclean: distclean

devps: FORCE
	@echo Making all in devps
	@cd devps; $(MAKE) "FONTDIR=$(FONTDIR)" "PAGE=$(PAGE)" all

FORCE:
