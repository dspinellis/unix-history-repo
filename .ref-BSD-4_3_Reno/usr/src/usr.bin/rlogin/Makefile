#	@(#)Makefile	5.5 (Berkeley) 5/11/90

PROG=	rlogin
SRCS=	rlogin.c krcmd.c kcmd.c des_rw.c
DPADD=	${LIBKRB} ${LIBDES}
CFLAGS+=-DKERBEROS
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
