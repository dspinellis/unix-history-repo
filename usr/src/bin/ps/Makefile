#	@(#)Makefile	5.9 (Berkeley) %G%

PROG=	ps
SRCS=	devname.c fmt.c keyword.c nlist.c print.c ps.c
CFLAGS+=-I/sys
DPADD=	${LIBMATH} ${LIBKVM} ${LIBUTIL}
LDADD=	-lm -lkvm -lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
