#	@(#)Makefile	5.10 (Berkeley) %G%

PROG=	fstat
CFLAGS+=-I/sys
DPADD=	${LIBKVM}
LDADD=	-lkvm
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
