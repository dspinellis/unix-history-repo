#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	uupoll
CFLAGS+=-I${.CURDIR}/../includes
BINMODE=6555
DPADD=	${LIBCOMPAT}
LDADD=	${LIBUU} -lcompat
MAN8=	uupoll.0

.include <bsd.prog.mk>
