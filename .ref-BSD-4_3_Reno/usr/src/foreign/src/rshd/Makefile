#	@(#)Makefile	5.6.1.1 (Berkeley) 10/21/90

PROG=	rshd
SRCS=	rshd.c
#SRCS=	rshd.c des_rw.c
#CFLAGS+=-DKERBEROS -DCRYPT
MAN8=	rshd.0
#DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
#LDADD=	-lutil -lkrb -ldes
DPADD=	${LIBUTIL}
LDADD=	-lutil
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
