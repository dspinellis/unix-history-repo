#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	ext_srvtab
CFLAGS+=-DKERBEROS -I${.CURDIR}/../include
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
NOMAN=	noman

.include <bsd.prog.mk>
