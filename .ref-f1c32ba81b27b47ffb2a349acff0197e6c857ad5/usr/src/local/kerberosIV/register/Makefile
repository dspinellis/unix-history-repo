#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	register
SRCS=	register.c des_rw.c
CFLAGS+=-DCRYPT -DDEBUG -DKERBEROS -I${.CURDIR}/../include
.PATH:	${.CURDIR}/../../usr.bin/rlogin
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINDIR=	/usr/bin
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
