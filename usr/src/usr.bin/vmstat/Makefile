#	@(#)Makefile	5.8 (Berkeley) %G%

PROG=	vmstat
CFLAGS+=-I/sys
MAN8=	vmstat.0
BINGRP=	kmem
BINMODE=2555
DPADD=	names.c ${LIBUTIL}
LDADD=	-lkvm

.include <bsd.prog.mk>
