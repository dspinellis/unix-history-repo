#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
OBJS=	host.o inet.o if.o main.o mbuf.o route.o unix.o
CFLAGS=-O
DESTDIR=

netstat: ${OBJS}
	${CC} ${OBJS} -o netstat

install: netstat
	install -s -m 2755 -g kmem netstat ${DESTDIR}/usr/ucb/netstat

clean:
	rm -f *.o core a.out errs
