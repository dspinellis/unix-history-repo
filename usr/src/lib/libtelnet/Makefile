#
# Copyright (c) 1991 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#

LIB=	telnet
SRCS=	auth.c encrypt.c genget.c getent.c gettytab.c misc.c
SRCS+=	kerberos.c krb_des.c
#SRCS+=	kerberos5.c simple_auth.c
CFLAGS+= -DENCRYPT -DAUTHENTICATE
CFLAGS+= -DKRB4 -DKRBDES_ENCRYPT -I/usr/include/kerberosIV

.PATH:  ${.CURDIR}/../../libexec/getty
.include <bsd.lib.mk>

