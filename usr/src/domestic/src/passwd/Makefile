#	@(#)Makefile	5.7 (Berkeley) 6/19/90

PROG=	passwd
SRCS=	passwd.c des_rw.c
DPADD=	${LIBKRB} ${LIBDES}
CFLAGS+=-I${.CURDIR} -DKERBEROS
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../rlogin

.include <bsd.prog.mk>
