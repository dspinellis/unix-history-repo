#	@(#)Makefile	8.2 (Berkeley) %G%

PROG=	mountd
CFLAGS+=-DNFS -DMFS -DISOFS
MAN5=	exports.0 netgroup.0
MAN8=	mountd.0
DPADD=	${LIBRPC}
LDADD=	-lrpc

.include <bsd.prog.mk>
