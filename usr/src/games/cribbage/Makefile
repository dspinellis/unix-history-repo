#	@(#)Makefile	5.11 (Berkeley) 5/11/90

PROG=	cribbage
DPADD=	${LIBCURSES} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lcurses -ltermlib -lcompat
SRCS=	extern.c crib.c cards.c instr.c io.c score.c support.c
MAN6=	cribbage.0
HIDEGAME=hidegame

beforeinstall: 
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/cribbage.n \
	    ${DESTDIR}/usr/share/games/cribbage.instr

.include <bsd.prog.mk>
