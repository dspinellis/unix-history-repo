#	@(#)Makefile	5.9 (Berkeley) 9/27/90

PROG=	rlogind
CFLAGS+=-DKERBEROS -DCRYPT
SRCS=	rlogind.c des_rw.c
MAN8=	rlogind.0
DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
LDADD=	-lutil -lkrb -ldes
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
