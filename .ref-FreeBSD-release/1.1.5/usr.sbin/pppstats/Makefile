#
#	$Id$
#
PROG=	pppstat
SRCS=	pppstats.c

CFLAGS=	-DNO_DRAND48 -DKVMLIB -DPPP

DPADD+=	${LIBUTIL}
LDADD+=	-lutil

NOMAN=	noman

BINGRP=	kmem

.include <bsd.prog.mk>
