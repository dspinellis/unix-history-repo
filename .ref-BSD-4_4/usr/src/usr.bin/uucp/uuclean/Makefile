#	@(#)Makefile	8.1 (Berkeley) 6/6/93

PROG=	uuclean
CFLAGS+=-I${.CURDIR}/../includes
BINDIR=	${LIBDIR}
BINMODE=6111
DPADD=	${LIBCOMPAT}
LDADD=	${LIBUU} -lcompat
MAN8=	uuclean.0

.include <bsd.prog.mk>
