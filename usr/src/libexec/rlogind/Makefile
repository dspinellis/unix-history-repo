#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	rlogind
SRCS=	rlogind.c
#SRCS=	rlogind.c des_rw.c
#CFLAGS+=-DKERBEROS -DCRYPT
MAN8=	rlogind.0
#DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
#LDADD=	-lutil -lkrb -ldes
DPADD=	${LIBUTIL}
LDADD=	-lutil
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
