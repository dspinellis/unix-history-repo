#	@(#)Makefile	5.11.1.1 (Berkeley) %G%

PROG=	passwd
SRCS=	passwd.c
CFLAGS+=-I${.CURDIR}
#SRCS=	passwd.c des_rw.c
#CFLAGS+=-I${.CURDIR} -DKERBEROS -DCRYPT
#DPADD=	${LIBKRB} ${LIBDES}
#LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
