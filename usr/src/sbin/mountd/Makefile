#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	mountd
CFLAGS+=-DNFS
MAN5=	exports.0 netgroup.0
MAN8=	mountd.0
DPADD=	${LIBRPC}
LDADD=	-lrpc -lutil

.include <bsd.prog.mk>
