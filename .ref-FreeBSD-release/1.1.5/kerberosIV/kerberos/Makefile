#	From: @(#)Makefile	5.1 (Berkeley) 6/25/90
#	$Id$

PROG=	kerberos
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include 
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-L${KDBOBJDIR} -lkdb -L${KRBOBJDIR} -lkrb -L${DESOBJDIR} -ldes
NOMAN=	noman

.include <bsd.prog.mk>
