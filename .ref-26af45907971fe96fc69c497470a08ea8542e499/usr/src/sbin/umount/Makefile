#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	umount
CFLAGS+=-DNFS
NOMAN=	noman
DPADD=	${LIBRPC}
LDADD=	-lrpc

.include <bsd.prog.mk>
