#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)Makefile	5.7 (Berkeley) %G%
#

PROG=	telnet

CFLAGS+=-DTERMCAP -DKLUDGELINEMODE -DUSE_TERMIO -DAUTHENTICATION -DENCRYPTION
CFLAGS+=-I${.CURDIR}/../../lib

CFLAGS+= -DKRB4

LDADD=	-ltermcap -ltelnet
LDADD+=	-lkrb -ldes
DPADD=	${LIBTERMCAP}

SRCS=	authenc.c commands.c main.c network.c ring.c sys_bsd.c telnet.c \
	terminal.c tn3270.c utilities.c

.include <bsd.prog.mk>
