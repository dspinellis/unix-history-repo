#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
#
# optional flags are: MEASURE TESTING DEBUG
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCDS=	acksend.c candidate.c correct.c master.c networkdelta.c readmsg.c \
	slave.c timed.c
OBJDS=	acksend.o candidate.o correct.o master.o networkdelta.o readmsg.o \
	slave.o timed.o
SRCCS=	cmds.c cmdtab.c timedc.c
OBJCS=	cmds.o cmdtab.o timedc.o
COMMS=	byteorder.c measure.c cksum.${MACHINE}.c
COMMO=	byteorder.o measure.o cksum.${MACHINE}.o
MAN=	timedc.0 timed.0

all: timed timedc

timed: ${OBJDS} ${COMMO} ${LIBC}
	${CC} ${CFLAGS} ${OBJDS} ${COMMO} ${LIBS} -o $@ -lutil

timedc: ${OBJCS} ${COMMO} ${LIBC}
	${CC} ${CFLAGS} ${OBJCS} ${COMMO} ${LIBS} -o $@ -lutil

cksum.${MACHINE}.o:
	${CC} ${CFLAGS} -c $*.c

clean:
	rm -f ${OBJDS} ${OBJCS} ${COMMO} core timed timedc

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCDS} ${SRCCS} ${COMMS}

install: ${MAN}
	install -s -o bin -g bin -m 755 timed ${DESTDIR}/etc/timed
	install -s -o root -g bin -m 4511 timedc ${DESTDIR}/etc/timedc
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCDS} ${COMMS}
	lint ${CFLAGS} ${SRCCS} ${COMMS}

tags: ${SRCS}
	ctags ${SRCDS} ${SRCCS} ${COMMS}
