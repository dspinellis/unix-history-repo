#	@(#)Makefile	5.4 (Berkeley) 5/7/91

LIB=	telnet
SRCS=	auth.c encrypt.c genget.c getent.c gettytab.c misc.c
SRCS+=	kerberos.c enc_des.c
CFLAGS+= -I/usr/include/kerberosIV

.PATH:  ${.CURDIR}/../../libexec/getty
.include <bsd.lib.mk>

