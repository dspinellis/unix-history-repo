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
CFLAGS=-g -O -Wall -Wcast-qual -Wwrite-strings
LDFLAGS=-g
OLDCC=gcc
MLIBS=-lm
INCLUDES=-I../driver -I../lib
DEFINES=
SOURCES=dvi.c
MISC=Makefile devgps
BINDIR=/usr/local/bin
FONTDIR=/usr/local/lib/groff/font
MACRODIR=/usr/local/lib/groff/tmac
ETAGS=etags
ETAGSFLAGS=-p

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: grodvi tfmtodit devdvi

grodvi: dvi.o ../driver/libdriver.a ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ dvi.o \
	../driver/libdriver.a ../lib/libgroff.a $(MLIBS)

tfmtodit: tfmtodit.o ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ tfmtodit.o ../lib/libgroff.a $(MLIBS)

dvi.o: ../driver/printer.h ../driver/driver.h ../lib/font.h

install.bin: grodvi tfmtodit
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp grodvi $(BINDIR)
	cp tfmtodit $(BINDIR)
	@echo Making install.bin in devdvi
	@cd devdvi; $(MAKE) "FONTDIR=$(FONTDIR)" install.bin

install.nobin:
	-[ -d $(MACRODIR) ] || mkdir $(MACRODIR)
	cp tmac.dvi $(MACRODIR)
	@echo Making install.nobin in devdvi
	@cd devdvi; $(MAKE) "FONTDIR=$(FONTDIR)" install.nobin

TAGS: dvi.c
	$(ETAGS) $(ETAGSFLAGS) dvi.c

clean:
	-rm -f *.o core grodvi tfmtodit

distclean: clean
	-rm -f TAGS

realclean: distclean

devdvi: FORCE
	@echo Making all in $@
	@cd $@; $(MAKE) all

FORCE:
