#	@(#)Makefile	5.3 (Berkeley) 5/11/90

PROG=	ps
SRCS=	ps.c devname.c attime.c proc_compare.c
DPADD=	${LIBMATH} ${LIBUTIL}
LDADD=	-lm -lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
