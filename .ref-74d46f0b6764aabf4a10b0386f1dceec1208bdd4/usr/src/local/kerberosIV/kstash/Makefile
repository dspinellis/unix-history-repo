#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	kstash
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes 
NOMAN=	noman

.include <bsd.prog.mk>
