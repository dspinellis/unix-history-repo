#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	bc
SRCS=	bc.c
CFLAGS+=-w -I${.CURDIR}
CLEANFILES=	bc.c y.tab.h

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/bc.library \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
