#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	5.2 (Berkeley) 6/26/90
#
PROG=	registerd
SRCS=	registerd.c des_rw.c
CFLAGS+=-DKERBEROS -I${.CURDIR}/../register
.PATH:	${.CURDIR}/../../usr.bin/rlogin
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
MAN8=	registerd.0

.include <bsd.prog.mk>
