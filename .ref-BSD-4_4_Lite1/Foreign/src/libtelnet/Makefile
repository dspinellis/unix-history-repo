#	@(#)Makefile	8.2 (Berkeley) 12/15/93

LIB=	telnet
SRCS=	auth.c encrypt.c genget.c getent.c misc.c
SRCS+=	kerberos.c enc_des.c
#SRCS+=	kerberos5.c
CFLAGS+= -DENCRYPTION -DAUTHENTICATION -DHAS_CGETENT
CFLAGS+= -DKRB4 -DDES_ENCRYPTION -I/usr/include/kerberosIV

# These are the sources that have encryption stuff in them.
CRYPT_SRC= auth.c enc-proto.h enc_des.c encrypt.c
CRYPT_SRC+= encrypt.h kerberos.c kerberos5.c krb4encpwd.c
CRYPT_SRC+= misc.c spx.c Makefile
NOCRYPT_DIR=${.CURDIR}/Nocrypt

.include <bsd.lib.mk>

nocrypt:
	@echo "Encryption code already removed."
