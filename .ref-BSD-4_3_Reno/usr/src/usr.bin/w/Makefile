#	@(#)Makefile	5.4 (Berkeley) 5/11/90

PROG=	w
SRCS=	w.c attime.c proc_compare.c
MAN1=	w.0 uptime.0
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINGRP=	kmem
BINMODE=2555
LINKS=	${BINDIR}/w ${BINDIR}/uptime
.PATH:	${.CURDIR}/../../bin/ps

.include <bsd.prog.mk>
