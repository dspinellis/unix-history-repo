#	@(#)Makefile	5.5 (Berkeley) 6/23/90

PROG=	bc
SRCS=	bc.c
CFLAGS+=-I${.CURDIR}
CLEANFILES=	bc.c y.tab.h

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/bc.library \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
