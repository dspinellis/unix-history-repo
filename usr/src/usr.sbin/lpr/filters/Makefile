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
#	@(#)Makefile	5.4 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBDIR=	/usr/lib
SRCS=	lpf.c
OBJS=	lpf.o

all: lpf

lpf: lpf.c
	${CC} -o $@ ${CFLAGS} lpf.c

clean: FRC
	rm -f ${OBJS} core lpf

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 lpf ${DESTDIR}/${LIBDIR}/lpf

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

lpf: lpf.c /usr/include/stdio.h /usr/include/signal.h
lpf: /usr/include/machine/trap.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
