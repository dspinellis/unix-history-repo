# Copyright 1985, Massachusetts Institute of Technology
#
#	 - makefile for keycomp
#

DESTDIR =
CONFDIR = /usr/new
INCDIR = -I../include

CFLAGS = -O ${INCDIR}
XLIB = ../Xlib/libX.a
SRCS = keycomp.c
OBJS = keycomp.o
PROGRAM = keycomp

all: keycomp Xkeymap

clean: 
	rm -f *.bak *.o *~ $(PROGRAM) Xkeymap \#*

Xkeymap: Xkeymap.default keycomp
	keycomp <Xkeymap.default >Xkeymap

install: all
	install keycomp ${DESTDIR}${CONFDIR}
	cp Xkeymap.default ${DESTDIR}/usr/lib/Xkeymap.txt
	chmod 0644 ${DESTDIR}/usr/lib/Xkeymap.txt

keycomp: keycomp.o ../Xlib/libX.a
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB)

$(OBJS): $(SRCS)

