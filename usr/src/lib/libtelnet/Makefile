#	@(#)Makefile	5.6 (Berkeley) %G%

LIB=	telnet
SRCS=	auth.c encrypt.c genget.c getent.c gettytab.c misc.c
SRCS+=	kerberos.c enc_des.c
#SRCS+=	kerberos5.c
CFLAGS+= -DENCRYPTION -DAUTHENTICATION
CFLAGS+= -DKRB4 -DDES_ENCRYPTION -I/usr/include/kerberosIV

.PATH:  ${.CURDIR}/../../libexec/getty
.include <bsd.lib.mk>

