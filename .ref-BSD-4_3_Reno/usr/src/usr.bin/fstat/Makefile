#	@(#)Makefile	5.4 (Berkeley) 5/11/90

PROG=	fstat
SRCS=	fstat.c devname.c
.PATH:	${.CURDIR}/../../bin/ps
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
