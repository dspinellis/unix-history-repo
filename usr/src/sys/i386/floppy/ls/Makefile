#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	ls
SRCS=	cmp.c ls.c print.c util.c
CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../bin/ls

.include <bsd.prog.mk>
