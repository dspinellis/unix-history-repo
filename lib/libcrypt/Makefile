#	@(#)Makefile	5.4 (Berkeley) 5/7/91

LIB=	crypt
SRCS=	auth.c crypt.c encrypt.c misc.c
#SHLIB_MAJOR= 0
#SHLIB_MINOR= 0

# Do DES encryption?
CFLAGS+= -DDES_ENCRYPT
SRCS+=	enc_des.c

# Do Kerberos encryption?
#CFLAGS+= -DKRBDES_ENCRYPT
#SRCS+=	krb_des.c

# Kerberos 4?
#CFLAGS+=-DKRB4
#SRCS+=	kerberos.c

# Kerberos 5?
#CFLAGS+= -DKRB5
#SRCS+=	kerberos5.c

CFLAGS+= -I/usr/include/kerberosIV -DAUTHENTICATE -DENCRYPT

.PATH:  ${.CURDIR}/../../libexec/getty
.include <bsd.lib.mk>

