#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	tetris
SRCS=	input.c screen.c shapes.c scores.c tetris.c
MAN6=	tetris.0
DPADD=	${LIBTERM}
LDADD=	-ltermcap
HIDEGAME=hidegame

all: ${PROG}

.include <bsd.prog.mk>
