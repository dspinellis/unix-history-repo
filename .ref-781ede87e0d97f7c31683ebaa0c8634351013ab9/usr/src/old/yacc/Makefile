#	@(#)Makefile	4.7 (Berkeley) %G%

PROG=	yacc
CFLAGS+=-DWORD32
SRCS=	y1.c y2.c y3.c y4.c
MAN1=	yacc.0

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/old.yaccpar \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
