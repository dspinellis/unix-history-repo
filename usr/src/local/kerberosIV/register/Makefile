#	@(#)Makefile	1.6 (Berkeley) %G%

PROG=	register
SRCS=	register.c des_rw.c
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:  ${.CURDIR}/../../usr.bin/rlogin
BINDIR=	/usr/bin

.include <bsd.prog.mk>
