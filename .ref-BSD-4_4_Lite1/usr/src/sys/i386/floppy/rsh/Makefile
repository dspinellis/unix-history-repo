#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	rsh
SRCS=   rsh.c
CFLAGS+=-DLETS_GET_SMALL
BINOWN=	root
BINMODE=4555
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../usr.bin/rsh ${.CURDIR}/../../../../usr.bin/rlogin

.include <bsd.prog.mk>
