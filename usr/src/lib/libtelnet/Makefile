#
# Copyright (c) 1991 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#

LIB=	telnet
SRCS=	pow.c gcd.c msqrt.c mdiv.c mout.c mult.c madd.c util.c
SRCS=	auth.c encrypt.c genget.c getent.c kerberos.c \
	kerberos5.c krb_des.c misc.c simple_auth.c
CFLAGS+= -DENCRYPT -DAUTHENTICATE

.include <bsd.lib.mk>
