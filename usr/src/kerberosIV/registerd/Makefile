#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	8.1 (Berkeley) 6/1/93
#
PROG=	registerd
SRCS=	registerd.c des_rw.c
CFLAGS+=-DCRYPT -DKERBEROS -I${.CURDIR}/../register
.PATH:	${.CURDIR}/../../usr.bin/rlogin
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
MAN8=	registerd.0
BINDIR=	/usr/libexec

.include <bsd.prog.mk>
