#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	nfsstat
CFLAGS+=-DNFS
DPADD=  ${LIBKVM}
LDADD= -lkvm
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
