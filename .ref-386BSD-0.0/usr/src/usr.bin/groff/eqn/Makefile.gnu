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
# default device
DEVICE=ps
CFLAGS=-g -O -Wall
CC=g++
INCLUDES=-I../lib
DEFINES=-DDEVICE=\"$(DEVICE)\"
YACC=bison -y
YACCFLAGS=-v
ETAGS=etags
ETAGSFLAGS=-p
OBJECTS=eqn.tab.o main.o lex.o  box.o limit.o list.o over.o text.o \
	script.o mark.o other.o delim.o sqrt.o pile.o
SOURCES=main.c lex.c eqn.y box.c limit.c list.c over.c text.c \
	script.c mark.c other.c delim.c sqrt.c pile.c \
	eqn.h box.h pbox.h

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: eqn

eqn: $(OBJECTS) ../lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) ../lib/libgroff.a $(LIBS)

eqn.tab.c: eqn.y
	$(YACC) $(YACCFLAGS) -d eqn.y
	mv y.tab.c eqn.tab.c
	mv y.tab.h eqn.tab.h

eqn.tab.o: eqn.h box.h
box.o: eqn.h box.h pbox.h
limit.o: eqn.h box.h pbox.h
text.o: eqn.h box.h pbox.h
over.o: eqn.h box.h pbox.h
list.o: eqn.h box.h pbox.h
script.o: eqn.h box.h pbox.h
mark.o: eqn.h box.h pbox.h
other.o: eqn.h box.h pbox.h
delim.o: eqn.h box.h pbox.h
sqrt.o: eqn.h box.h pbox.h
pile.o: eqn.h box.h pbox.h
main.o: eqn.h  box.h  ../lib/stringclass.h
lex.o: eqn.h eqn.tab.c box.h ../lib/stringclass.h

install.bin: eqn
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp eqn $(BINDIR)/geqn

install.nobin:

install: install.bin install.nobin

TAGS: $(SOURCES)
	$(ETAGS) $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o core eqn gmon.out

distclean: clean
	-rm -f TAGS eqn.output y.output

realclean: distclean
	-rm -f eqn.tab.c eqn.tab.h
