#	@(#)Makefile	8.1 (Berkeley) 5/31/93

PROG=	tetris
SRCS=	input.c screen.c shapes.c scores.c tetris.c
MAN6=	tetris.0
DPADD=	${LIBTERM}
LDADD=	-ltermcap
HIDEGAME=hidegame

.include <bsd.prog.mk>
