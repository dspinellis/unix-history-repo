#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	iostat
CFLAGS+=-I/sys -I${.CURDIR}/../../usr.bin/vmstat
MAN8=	iostat.0
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINGRP=	kmem
BINMODE=2555

.include <bsd.prog.mk>
