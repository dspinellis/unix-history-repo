#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	gomoku
SRCS=	bdinit.c bdisp.c main.c makemove.c pickmove.c stoc.c
MAN6=	gomoku.0
DPADD=	${LIBCURSES} ${LIBTERM}
LDADD=	-lcurses -ltermlib
HIDEGAME=hidegame

.include <bsd.prog.mk>
