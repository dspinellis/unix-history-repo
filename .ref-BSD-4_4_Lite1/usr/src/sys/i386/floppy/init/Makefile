#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	init
SRCS=	init.c
CFLAGS+=-DLETS_GET_SMALL
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINMODE=500
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/init

.include <bsd.prog.mk>
