#	BSDI $Id: Makefile,v 1.3 1993/06/14 20:06:22 donn Exp $
#	@(#)Makefile	5.3 (Berkeley) 5/11/90

PROG=	init
MAN8=	init.0
# XXX fix this when we have KERN_SECURELVL
CFLAGS+=-DSECURE -DNOSYSCTL
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINMODE=500

.include <bsd.prog.mk>
