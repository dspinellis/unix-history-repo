#	@(#)Makefile	5.6.1.1 (Berkeley) 10/21/90

#CFLAGS+=-DKERBEROS
PROG=	login
SRCS=	login.c
#SRCS=	klogin.c login.c
#DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
#LDADD=	-lutil -lkrb -ldes
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
