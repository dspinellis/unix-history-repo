#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.7	(Berkeley)	7/14/87
#
CFLAGS=	-O -I.
LIBC=	/lib/libc.a
SRCS=	dc.c
OBJS=

all: dc

dc: dc.c ${LIBC}
	${CC} ${CFLAGS} dc.c -o $@

clean: FRC
	rm -f ${OBJS} core dc

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 dc ${DESTDIR}/usr/bin/dc

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dc: dc.c /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
dc: dc.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
