#	BSDI $Id: Makefile,v 1.1 1993/08/02 23:17:24 nate Exp $
#	@(#)Makefile	5.3 (Berkeley) 5/11/90

PROG=	init
MAN8=	init.8
# XXX fix this when we have KERN_SECURELVL
CFLAGS+=-DSECURE -DNOSYSCTL
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINMODE=500

.include <bsd.prog.mk>
