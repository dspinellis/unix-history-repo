#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	wump
MAN6=	wump.0
HIDEGAME=hidegame

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/wump.info \
	    ${DESTDIR}/usr/share/games

.include <bsd.prog.mk>
