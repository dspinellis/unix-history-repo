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
OLDCFLAGS=-g
MLIBS=-lm
INCLUDES=-I../driver -I../lib
DEFINES=
BINDIR=/usr/local/bin
FONTDIR=/usr/local/lib/groff/font
MACRODIR=/usr/local/lib/groff/tmac
ETAGS=etags
ETAGSFLAGS=-p
DEVICES=devascii devlatin1

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: grotty $(DEVICES)

grotty: tty.o ../driver/libdriver.a ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ tty.o \
	../driver/libdriver.a ../lib/libgroff.a $(MLIBS)

tty.o : ../driver/printer.h ../driver/driver.h ../lib/font.h

install.bin: grotty
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp grotty $(BINDIR)
	@for dir in $(DEVICES); do \
	echo Making install.bin in $$dir; \
	(cd $$dir; $(MAKE) "FONTDIR=$(FONTDIR)" install.bin); done

install.nobin:
	-[ -d $(MACRODIR) ] || mkdir $(MACRODIR)
	cp tmac.tty $(MACRODIR)
	@for dir in $(DEVICES); do \
	echo Making install.nobin in $$dir; \
	(cd $$dir; $(MAKE) "FONTDIR=$(FONTDIR)" install.nobin); done

install: install.bin install.nobin

clean:
	-rm -f *.o core grotty
	@for dir in $(DEVICES); do \
	echo Making clean in $$dir; \
	(cd $$dir; $(MAKE) clean); done
	

distclean: clean
	-rm -f TAGS

realclean: distclean

TAGS: tty.c
	$(ETAGS) $(ETAGSFLAGS) tty.c

$(DEVICES): FORCE
	@echo Making all in $@
	@cd $@; $(MAKE) all
FORCE:
