#	@(#)Makefile	5.6 (Berkeley) 6/24/90

#CFLAGS+=-DKERBEROS
PROG=	login
SRCS=	klogin.c login.c
#DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
#LDADD=	-lutil -lkrb -ldes
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
