#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#
DESTDIR=
CFLAGS=-O
tset:	tset.c tset.delays.h
	${CC} ${CFLAGS} tset.c -o tset -ltermlib

install: tset
	install -s tset ${DESTDIR}/usr/ucb
	rm -f ${DESTDIR}/usr/ucb/reset
	ln ${DESTDIR}/usr/ucb/tset ${DESTDIR}/usr/ucb/reset

clean:
	rm -f *.o tset reset

depend:
