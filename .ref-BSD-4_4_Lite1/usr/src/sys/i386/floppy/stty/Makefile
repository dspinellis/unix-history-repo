#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	stty
SRCS=	cchar.c gfmt.c modes.c print.c stty.c util.c
CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../bin/stty

.include <bsd.prog.mk>
