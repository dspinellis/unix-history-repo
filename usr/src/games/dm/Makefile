#	@(#)Makefile	5.8 (Berkeley) %G%

# -DLOG		log games
PROG=	dm
MAN8=	dm.0
MAN5=	dm.conf.0
BINGRP=	kmem
BINMODE=6555
DPADD=	${LIBUTIL} ${LIBKVM}
LDADD=	-lutil -lkvm

.include <bsd.prog.mk>
