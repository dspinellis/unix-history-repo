#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	rsh
SRCS=	rsh.c
#CFLAGS+=-DKERBEROS -DCRYPT
#SRCS=	rsh.c krcmd.c kcmd.c des_rw.c
#DPADD=	${LIBKRB} ${LIBDES}
#LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
INSTALLFLAGS=-fschg
.PATH:	${.CURDIR}/../rlogin

.include <bsd.prog.mk>
