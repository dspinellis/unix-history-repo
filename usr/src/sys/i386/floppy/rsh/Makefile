#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	rsh
SRCS=   rsh.c krcmd.c kcmd.c des_rw.c
CFLAGS+=-DLETS_GET_SMALL -DKERBEROS -DCRYPT
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../usr.bin/rsh ${.CURDIR}/../../../../usr.bin/rlogin

.include <bsd.prog.mk>
