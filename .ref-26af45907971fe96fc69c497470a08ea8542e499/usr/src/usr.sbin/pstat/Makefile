#	@(#)Makefile	5.7 (Berkeley) %G%

PROG=	pstat
CFLAGS+=-I/sys
BINGRP=	kmem
BINMODE=2555
DPADD=	${LIBKVM}
LDADD=	-lkvm

.include <bsd.prog.mk>
