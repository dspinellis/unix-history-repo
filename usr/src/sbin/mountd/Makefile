#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	mountd
CFLAGS+=-DNFS
MAN8=	mountd.0
DPADD=	${LIBRPC}
LDADD=	-lrpc

.include <bsd.prog.mk>
