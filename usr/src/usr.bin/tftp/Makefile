#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
ALL=	tftp
DESTDIR=
CFLAGS=-O

all: ${ALL}

tftp:	main.o tftp.o tftpsubs.o
	${CC} main.o tftp.o tftpsubs.o -o tftp

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

depend:

install: 
	install -s tftp ${DESTDIR}/usr/ucb
