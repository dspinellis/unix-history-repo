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

CC=g++
AR=ar
RANLIB=/bin/ranlib
INCLUDES=-I../lib
DEFINES=
MALLOC=malloc.o
MALLOCFLAGS=
OBJECTS=input.o printer.o
SOURCES=input.c printer.c printer.h driver.h

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: libdriver.a

libdriver.a: $(OBJECTS)
	$(AR) r libdriver.a $?
	if test "$(RANLIB)" ; then $(RANLIB) libdriver.a ;fi

$(OBJECTS):  printer.h driver.h

TAGS : $(SOURCES)
	etags $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o core libdriver.a

distclean: clean
	-rm -f TAGS

realclean: distclean

install.bin:
install.nobin:
install:
