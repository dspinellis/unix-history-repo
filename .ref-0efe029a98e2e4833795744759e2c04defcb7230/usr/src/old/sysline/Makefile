#	@(#)Makefile	5.8 (Berkeley) %G%

PROG=	sysline
DPADD=	${LIBTERMCAP} ${LIBCURSES} ${LIBUTIL}
LDADD=	-ltermcap -lcurses -lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
