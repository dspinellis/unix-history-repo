#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	klist
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINDIR=	/usr/bin
NOMAN=	noman

.include <bsd.prog.mk>
