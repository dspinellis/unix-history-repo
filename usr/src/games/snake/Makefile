#	@(#)Makefile	5.10 (Berkeley) 5/11/90

PROG=	snake
SRCS=	snake.c move.c
MAN6=	snake.0
DPADD=	${LIBM} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lm -ltermcap -lcompat
SUBDIR=	snscore
HIDEGAME=hidegame

.include <bsd.prog.mk>
