#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	tset.c
OBJS=	tset.o

all: tset

tset:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermlib

clean: FRC
	rm -f ${OBJS} core tset

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 tset ${DESTDIR}/usr/ucb/tset
	rm -f ${DESTDIR}/usr/ucb/reset
	ln ${DESTDIR}/usr/ucb/tset ${DESTDIR}/usr/ucb/reset

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

tset.o: tset.c /usr/include/ttyent.h /usr/include/sgtty.h
tset.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tset.o: /usr/include/sys/ttydev.h /usr/include/stdio.h /usr/include/signal.h
tset.o:tset.delays.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
