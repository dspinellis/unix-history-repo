#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	rdist
CFLAGS+=-I${.CURDIR}
SRCS=	docmd.c expand.c lookup.c main.c server.c
OBJS+=	gram.o
BINOWN=	root
BINMODE=4555
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
CLEANFILES=y.tab.h

.include <bsd.prog.mk>
