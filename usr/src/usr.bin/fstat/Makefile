#	@(#)Makefile	5.7 (Berkeley) %G%

PROG=	fstat
SRCS=	fstat.c devname.c
.PATH:	${.CURDIR}/../../bin/ps
CFLAGS+=-I/sys
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
