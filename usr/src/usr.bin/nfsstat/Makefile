#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	nfsstat
CFLAGS+=-DNFS
DPADD=  ${LIBUTIL}
LDADD= -lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
