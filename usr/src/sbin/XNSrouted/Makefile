#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6 (Berkeley) %G%";
#
#	Makefile for Xerox Routing Information Protocol
#	Daemon
#
ALL=	XNSrouted
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o
CFLAGS=  -O
C2=	/lib/c2
ASMSCRIPT= /sys/machine/asm.sed
DESTDIR=
LDFLAGS=

all:	${ALL}

XNSrouted: ${OBJS}
	${CC} -g -o XNSrouted ${LDFLAGS} ${OBJS}

install: XNSrouted
	install -s XNSrouted ${DESTDIR}/etc

clean:
	rm -f XNSrouted *.o core a.out t.c linterrs typescript errs

depend:
