BINDIR=	/usr/local
PROG=	register
SRCS=	register.c des_rw.c
DPADD=	${LIBKRB} ${LIBDES}
CFLAGS+=-DKERBEROS
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:  ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
