#	@(#)Makefile	5.1 (Berkeley) 6/25/90

PROG=	ksrvtgt
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINDIR=	/usr/bin
NOMAN=	noman

.include <bsd.prog.mk>
