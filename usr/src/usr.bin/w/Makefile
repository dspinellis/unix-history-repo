#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	w
SRCS=	attime.c w.c proc_compare.c
MAN1=	w.0 uptime.0
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINGRP=	kmem
BINMODE=2555
LINKS=	${BINDIR}/w ${BINDIR}/uptime

.include <bsd.prog.mk>
