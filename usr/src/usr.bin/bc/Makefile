#	@(#)Makefile	5.4 (Berkeley) %G%

PROG=	bc
SRCS=	bc.c
CFLAGS+=-I${.CURDIR}
CLEANFILES=	bc.c

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/lib.b \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
