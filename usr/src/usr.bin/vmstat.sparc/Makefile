#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	vmstat
SRCS=	getdev.c intr.c main.c mem.c names.c sum.c util.c vmstat.c
MAN8=	vmstat.0
BINGRP=	kmem
BINMODE=2555
DPADD=	${LIBKVM}
LDADD=	-lkvm

.include <bsd.prog.mk>
