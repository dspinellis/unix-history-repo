#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	ps
SRCS=	devname.c keyword.c nlist.c print.c ps.c
CFLAGS+=-I/sys
DPADD=	${LIBMATH} ${LIBUTIL}
LDADD=	-lm -lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
