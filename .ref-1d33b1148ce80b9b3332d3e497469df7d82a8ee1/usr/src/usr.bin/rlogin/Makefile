#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	rlogin
SRCS=	rlogin.c
#SRCS=	rlogin.c krcmd.c kcmd.c des_rw.c
#DPADD=	${LIBKRB} ${LIBDES}
#CFLAGS+=-DKERBEROS
#LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
INSTALLFLAGS=-fschg

.include <bsd.prog.mk>
