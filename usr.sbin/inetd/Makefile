#	from: @(#)Makefile	5.5 (Berkeley) 6/29/90
#	$Id: Makefile,v 1.3 1993/09/23 17:31:35 jtc Exp $

PROG=	inetd
MAN8=	inetd.0
MLINKS=	inetd.8 inetd.5

DPADD=	${LIBUTIL} ${LIBRPC}
LDADD=	-lutil -lrpc

.include <bsd.prog.mk>
