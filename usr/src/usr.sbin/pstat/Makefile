#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	pstat
CFLAGS+= -I/sys
SRCS=	pstat.c devname.c
.PATH:	${.CURDIR}/../../bin/ps
MAN8=	pstat.0
DPADD= ${LIBUTIL}
LDADD=	-lutil -lkvm
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
