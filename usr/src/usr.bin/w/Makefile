#	@(#)Makefile	5.9 (Berkeley) %G%

PROG=	w
SRCS=	fmt.c pr_time.c proc_compare.c w.c
MAN1=	w.0 uptime.0
DPADD=	${LIBKVM}
LDADD=	-lkvm
BINGRP=	kmem
BINMODE=2555
LINKS=	${BINDIR}/w ${BINDIR}/uptime

.PATH: ${.CURDIR}/../../bin/ps

.include <bsd.prog.mk>
