#	@(#)Makefile	5.8 (Berkeley) %G%

PROG=	passwd
SRCS=	passwd.c des_rw.c
DPADD=	${LIBKRB} ${LIBDES}
CFLAGS+=-I${.CURDIR} -DKERBEROS -DCRYPT
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../rlogin

.include <bsd.prog.mk>
