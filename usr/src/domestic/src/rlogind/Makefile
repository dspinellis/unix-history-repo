#	@(#)Makefile	5.8 (Berkeley) 5/11/90

PROG=	rlogind
CFLAGS+=-DKERBEROS
SRCS=	rlogind.c des_rw.c
MAN8=	rlogind.0
DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
LDADD=	-lutil -lkrb -ldes
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
