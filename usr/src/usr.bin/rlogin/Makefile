#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	rlogin
SRCS=	rlogin.c krcmd.c kcmd.c des_rw.c
DPADD=	${LIBKRB} ${LIBDES}
CFLAGS+=-DKERBEROS -DCRYPT
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
