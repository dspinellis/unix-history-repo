#	@(#)Makefile	5.8.1.1 (Berkeley) 10/21/90

PROG=	passwd
SRCS=	passwd.c
CFLAGS+=-I${.CURDIR}
#SRCS=	passwd.c des_rw.c
#CFLAGS+=-I${.CURDIR} -DKERBEROS -DCRYPT
#DPADD=	${LIBKRB} ${LIBDES}
#LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../rlogin

.include <bsd.prog.mk>
