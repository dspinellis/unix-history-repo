#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	mount
SRCS=	mount.c
CFLAGS+=-DLETS_GET_SMALL -DNFS
NOMAN=	noman
DPADD=	${LIBRPC}
LDADD=	-lrpc
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../sbin/mount

.include <bsd.prog.mk>
