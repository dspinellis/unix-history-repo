#	@(#)Makefile	8.1 (Berkeley) 5/31/93

PROG=	chess
SRCS=	gnuchess.c uxdsp.c move.c
CFLAGS+=-DNEWMOVE=12
MAN6=	chess.0
DPADD=	${LIBCURSES} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lcurses -ltermlib -lcompat
HIDEGAME=hidegame

beforeinstall: 
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/gnuchess.book \
	    ${DESTDIR}/usr/share/games

.include <bsd.prog.mk>
