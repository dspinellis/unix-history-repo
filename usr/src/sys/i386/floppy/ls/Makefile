#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	ls
SRCS=	cmp.c ls.c print.c util.c
CFLAGS+=-DLETS_GET_SMALL
DPADD=	${LIBUTIL}
LDADD=	-lutil
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../bin/ls

.include <bsd.prog.mk>
