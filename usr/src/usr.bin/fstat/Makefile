#	@(#)Makefile	5.8 (Berkeley) %G%

PROG=	fstat
SRCS=	fstat.c devname.c
.PATH:	${.CURDIR}/../../bin/ps
CFLAGS+=-I/sys
DPADD=	${LIBKVM} ${LIBUTIL}
LDADD=	-lkvm -lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
