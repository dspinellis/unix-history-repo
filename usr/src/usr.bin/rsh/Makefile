#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	rsh
CFLAGS+=-DKERBEROS
SRCS=	rsh.c krcmd.c kcmd.c des_rw.c
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../rlogin

.include <bsd.prog.mk>
