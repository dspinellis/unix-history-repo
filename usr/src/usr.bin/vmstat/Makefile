#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	vmstat
CFLAGS+=-I/sys
BINGRP=	kmem
BINMODE=2555
DPADD=	names.c ${LIBUTIL}
LDADD=	-lutil

.include <bsd.prog.mk>
