#	@(#)Makefile	5.2 (Berkeley) 3/5/91

PROG=	kstash
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes 
NOMAN=	noman

.include <bsd.prog.mk>
