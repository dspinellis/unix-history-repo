#	@(#)Makefile	5.9 (Berkeley) 9/27/90

PROG=	rlogind
SRCS=	rlogind.c
MAN8=	rlogind.8
DPADD=	${LIBUTIL}
LDADD=	-lutil
.PATH: ${.CURDIR}/../../usr.bin/rlogin

.if exists(${LIBCRYPT})
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif
.if exists(${LIBKRB})
CFLAGS+=	-DKERBEROS
DPADD+= ${LIBKRB} ${LIBDES}
LDADD+= -lkrb -ldes
.endif

.include <bsd.prog.mk>
