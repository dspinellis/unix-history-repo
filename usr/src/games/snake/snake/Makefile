#	@(#)Makefile	5.10 (Berkeley) %G%

PROG=	snake
SRCS=	snake.c move.c
MAN6=	snake.0
DPADD=	${LIBM} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lm -ltermcap -lcompat
SUBDIR=	snscore
HIDEGAME=hidegame

.include <bsd.prog.mk>
