#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	fstat
SRCS=	fstat.c devname.c
CFLAGS+=-I/sys
.PATH:	${.CURDIR}/../../bin/ps
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
