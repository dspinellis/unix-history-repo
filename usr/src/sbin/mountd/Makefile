#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	mountd
CFLAGS+=-DNFS
MAN5=	exports.0 netgroup.0
MAN8=	mountd.0
DPADD=	${LIBRPC}
LDADD=	-lrpc

.include <bsd.prog.mk>
