
PROG=	more
SRCS=more.c
CFLAGS+=-DLETS_GET_SMALL
DPADD=	${LIBTERM}
LDADD=	-ltermcap
NOMAN=	noman
LDFLAGS=-s -N
.PATH:	${.CURDIR}/../../../../usr.bin/more

.include <bsd.prog.mk>
