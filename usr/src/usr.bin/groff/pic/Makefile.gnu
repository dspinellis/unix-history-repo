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
CFLAGS=-g -O -Wall
DEFINES=-DTEX_SUPPORT # -DFIG_SUPPORT 
INCLUDES=-I../lib 
MLIBS=-lm
YACCFLAGS=-v
YACC=bison -y
ETAGS=etags
ETAGSFLAGS=-p
GPERF=gperf
GPERFFLAGS=-p -a -t -k 1,3,4 -c -C -N lookup_keyword -T

OBJECTS=pic.tab.o lex.o main.o object.o common.o troff.o tex.o key.o # fig.o 
SOURCES=pic.y lex.c main.c object.c common.c troff.c tex.c key.c \
	pic.h text.h output.h object.h common.h key.h position.h
MISC=Makefile TODO pic.gperf

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: pic

pic: $(OBJECTS) ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) ../lib/libgroff.a $(MLIBS)

pic.tab.c: pic.y
	$(YACC) $(YACCFLAGS) -d pic.y
	mv y.tab.c pic.tab.c
	mv y.tab.h pic.tab.h

PIC_H= pic.h text.h output.h position.h \
       ../lib/lib.h ../lib/errarg.h ../lib/error.h ../lib/assert.h \
       ../lib/stringclass.h ../lib/cset.h 

pic.tab.o: $(PIC_H) object.h
object.o: $(PIC_H) object.h
troff.o: $(PIC_H) common.h
tex.o: $(PIC_H) common.h
# fig.o: $(PIC_H)
common.o: $(PIC_H) common.h
main.o: $(PIC_H)
lex.o: $(PIC_H) pic.tab.c object.h key.h
key.o: $(PIC_H) key.h pic.tab.c object.h 

key.c: pic.gperf
	$(GPERF) $(GPERFFLAGS) pic.gperf >$@

TAGS : $(SOURCES)
	$(ETAGS) $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o core pic

distclean: clean
	-rm -f pic.output y.output TAGS

realclean: distclean
	-rm -f pic.tab.c pic.tab.h key.c

install.bin: pic
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp pic $(BINDIR)/gpic

install.nobin:

install: install.bin install.nobin
