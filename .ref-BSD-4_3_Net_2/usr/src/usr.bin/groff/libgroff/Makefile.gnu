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

# FONTPATH says where to look for dev*/*
FONTPATH=.:/usr/local/lib/groff/font:/usr/local/lib/font:/usr/lib/font
OLDCC=gcc
CC=g++
AR=ar
RANLIB=/bin/ranlib
INCLUDES=-I.
DEFINES=-DFONTPATH=\"$(FONTPATH)\"
MALLOC=malloc.o
MALLOCFLAGS=
FMOD=#fmod.o
STRTOL=#strtol.o
OBJECTS=$(MALLOC) new.o itoa.o strerror.o error.o errarg.o fatal.o \
	strsave.o matherr.o assert.o iftoa.o string.o cset.o cmap.o \
	ptable.o font.o fontfile.o nametoindex.o filename.o lineno.o \
	progname.o lf.o change_lf.o version.o $(FMOD) $(STRTOL)
SOURCES=new.c itoa.c strerror.c error.c errarg.c fatal.c \
	strsave.c matherr.c fmod.c assert.c iftoa.c \
        string.c cset.c cmap.c ptable.c font.c fontfile.c nametoindex.c \
        filename.c lineno.c progname.c lf.c change_lf.c version.c \
	assert.h cset.h cmap.h errarg.h error.h font.h getpagesize.h \
	lib.h ptable.h stringclass.h

.c.o:
	$(CC) -c $(INCLUDES) $(CFLAGS) $(DEFINES) $<

all: libgroff.a

libgroff.a: $(OBJECTS)
	$(AR) r libgroff.a $?
	if test "$(RANLIB)" ; then $(RANLIB) libgroff.a ;fi

version.c: ../VERSION
	@echo Making version.c
	@echo const char \*version_string = \"`cat ../VERSION`\"\; >$@

malloc.o: malloc.c getpagesize.h
	$(OLDCC) $(OLDCFLAGS) $(MALLOCFLAGS) -c malloc.c

itoa.o: itoa.c
	$(OLDCC) $(OLDCFLAGS) -c itoa.c

iftoa.o: iftoa.c
	$(OLDCC) $(OLDCFLAGS) -c iftoa.c

strerror.o: strerror.c
	$(OLDCC) $(OLDCFLAGS) -c strerror.c

matherr.o: matherr.c
	$(OLDCC) $(OLDCFLAGS) -c matherr.c

fmod.o: fmod.c
	$(OLDCC) $(OLDCFLAGS) -c fmod.c

strtol.o: strtol.c
	$(OLDCC) $(OLDCFLAGS) -c strtol.c

string.o: stringclass.h
lf.o: stringclass.h

TAGS : $(SOURCES)
	etags $(ETAGSFLAGS) $(SOURCES)

clean:
	-rm -f *.o core libgroff.a version.c

distclean: clean
	-rm -f TAGS

realclean: distclean

install:
install.bin:
install.nobin:
