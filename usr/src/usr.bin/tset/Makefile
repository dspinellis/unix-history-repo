#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR=
CFLAGS=-O
tset:	tset.c
	${CC} ${CFLAGS} tset.c -o tset -ltermlib
install:
	install -s tset ${DESTDIR}/usr/ucb
clean:
	rm -f *.o tset
