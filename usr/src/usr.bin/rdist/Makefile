#	@(#)Makefile	5.9 (Berkeley) %G%

PROG=	rdist
CFLAGS+=-I${.CURDIR}
SRCS=	docmd.c expand.c lookup.c main.c server.c
OBJS+=	gram.o
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
