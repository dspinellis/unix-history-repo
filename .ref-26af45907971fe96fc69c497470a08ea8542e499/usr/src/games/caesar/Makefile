#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	caesar
MAN6=	caesar.0
DPADD=	${LIBM}
LDADD=	-lm
MLINKS=	caesar.6 rot13.6

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/rot13.sh ${DESTDIR}/usr/games/rot13

.include <bsd.prog.mk>
