#	@(#)Makefile	5.6 (Berkeley) 6/24/90

PROG=	login
SRCS=	klogin.c login.c
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINOWN=	root
BINMODE=4555

.if exists(${LIBCRYPT})
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif
.if exists(${LIBKRB})
DPADD+= ${LIBKRB} ${LIBDES}
LDADD+= -lkrb -ldes
CFLAGS+= -DKERBEROS
.endif

.include <bsd.prog.mk>
