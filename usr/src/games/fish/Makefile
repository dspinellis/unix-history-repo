#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	fish
MAN6=	fish.0
HIDEGAME=hidegame

beforeinstall: 
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/fish.instr \
	    ${DESTDIR}/usr/share/games/fish.instr

.include <bsd.prog.mk>
