#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	make_keypair
MAN8=	make_keypair.0
CFLAGS+=-DKERBEROS -I${.CURDIR}/../register
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes

.include <bsd.prog.mk>
