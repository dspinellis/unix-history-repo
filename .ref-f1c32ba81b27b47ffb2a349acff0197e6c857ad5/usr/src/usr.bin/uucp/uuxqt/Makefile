#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	uuxqt
CFLAGS+=-I${.CURDIR}/../includes
BINDIR=	${LIBDIR}
BINMODE=6550
MAN8=	uuxqt.0
DPADD=	${LIBCOMPAT}
LDADD=	${LIBUU} -lcompat

.include <bsd.prog.mk>
