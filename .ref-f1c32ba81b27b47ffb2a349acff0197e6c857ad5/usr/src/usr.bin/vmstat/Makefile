#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	vmstat
CFLAGS+=-I/sys
BINGRP=	kmem
BINMODE=2555
DPADD=	${LIBUTIL}
LDADD=	-lutil

.include <bsd.prog.mk>
