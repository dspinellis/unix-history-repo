#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	register
SRCS=	register.c des_rw.c
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
.PATH:	${.CURDIR}/../../usr.bin/rlogin
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINDIR=	/usr/bin
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
