#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	fstat
CFLAGS+=-I/sys
DPADD=	${LIBKVM}
LDADD=	-lkvm
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
