#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	5.4 (Berkeley) %G%
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
