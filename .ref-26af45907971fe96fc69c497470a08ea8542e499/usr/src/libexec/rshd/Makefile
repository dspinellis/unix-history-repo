#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	rshd
CFLAGS+=-DKERBEROS -DCRYPT
SRCS=	rshd.c des_rw.c
MAN8=	rshd.0
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
