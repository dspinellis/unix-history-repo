#	@(#)Makefile	4.6 (Berkeley) %G%

PROG=	lex
SRCS=	lmain.c parser.c sub1.c sub2.c header.c
CFLAGS+=-I${.CURDIR}

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${.CURDIR}/nrform \
	    ${.CURDIR}/ncform ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
