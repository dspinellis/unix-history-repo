#	@(#)Makefile	5.7 (Berkeley) %G%

PROG=	vmstat
CFLAGS+=-I/sys
MAN8=	vmstat.0
BINGRP=	kmem
BINMODE=2555
DPADD=	names.c ${LIBUTIL}
LDADD=	-lutil

.include <bsd.prog.mk>
