#	@(#)Makefile	5.14 (Berkeley) 6/18/90

PROG=	netstat
SRCS=	host.c inet.c if.c main.c mbuf.c route.c unix.c ns.c iso.c
BINGRP=	kmem
BINMODE=2555
LDADD=	-lutil
DPADD=	${LIBUTIL}

.include <bsd.prog.mk>
