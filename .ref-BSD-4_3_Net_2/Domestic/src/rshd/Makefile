#	@(#)Makefile	5.6 (Berkeley) 9/27/90

PROG=	rshd
CFLAGS+=-DKERBEROS -DCRYPT
SRCS=	rshd.c des_rw.c
MAN8=	rshd.0
DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
LDADD=	-lutil -lkrb -ldes
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
