#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	uucp
CFLAGS+=-I${.CURDIR}/../includes
BINMODE=6555
DPADD=	${LIBCOMPAT}
LDADD=	${LIBUU} -lcompat

.include <bsd.prog.mk>
