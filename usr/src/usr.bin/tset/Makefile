#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	tset.c

all: tset

tset: ${SRCS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS} -ltermlib

clean: FRC
	rm -f ${OBJS} core tset

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

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

tset: tset.c /usr/include/ttyent.h /usr/include/sgtty.h
tset: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tset: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
tset: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
tset: tset.delays.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
