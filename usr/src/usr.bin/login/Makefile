#	@(#)Makefile	5.6 (Berkeley) %G%

CFLAGS+=-DKERBEROS
PROG=	login
SRCS=	klogin.c login.c
DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
LDADD=	-lutil -lkrb -ldes
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
