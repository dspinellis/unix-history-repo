#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
PROG=	registerd
SRCS=	registerd.c des_rw.c
CFLAGS+=-DKERBEROS -I${.CURDIR}/../register
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
MAN8=	registerd.0

.include <bsd.prog.mk>
