#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	snake
SRCS=	snake.c move.c
MAN6=	snake.0
DPADD=	${LIBM} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lm -ltermcap -lcompat
HIDEGAME=hidegame

.include "../../Makefile.inc"
.include <bsd.prog.mk>
