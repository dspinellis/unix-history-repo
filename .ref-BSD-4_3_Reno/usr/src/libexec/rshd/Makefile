#	@(#)Makefile	5.5 (Berkeley) 5/11/90

PROG=	rshd
CFLAGS+=-DKERBEROS
SRCS=	rshd.c des_rw.c
MAN8=	rshd.0
DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
LDADD=	-lutil -lkrb -ldes
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
