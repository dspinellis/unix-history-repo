#	From: @(#)Makefile	5.2 (Berkeley) 3/5/91
#	$Id$

PROG=	kstash
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-L${KDBOBJDIR} -lkdb -L${KRBOBJDIR} -lkrb -L${DESOBJDIR} -ldes 
NOMAN=	noman

.include <bsd.prog.mk>
