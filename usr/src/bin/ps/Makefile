#	@(#)Makefile	5.10 (Berkeley) %G%

PROG=	ps
SRCS=	devname.c fmt.c keyword.c nlist.c print.c ps.c
CFLAGS+=-I/sys
DPADD=	${LIBMATH} ${LIBKVM}
LDADD=	-lm -lkvm
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
