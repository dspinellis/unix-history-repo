#	@(#)Makefile	1.2 (Berkeley) %G%

PROG=	kpasswdd
SRCS=	kpasswdd.c des_rw.c
CFLAGS+=-DKERBEROS -I${.CURDIR}/../../usr.bin/passwd
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
.PATH:	${.CURDIR}/../../usr.bin/rlogin
MAN8=	kpasswdd.0

.include <bsd.prog.mk>
