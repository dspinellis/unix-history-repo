#	From: @(#)Makefile	5.3 (Berkeley) 2/25/91
#	$Id$

PROG=	register
SRCS=	register.c des_rw.c
CFLAGS+=-DCRYPT -DDEBUG -DKERBEROS -I${.CURDIR}/../include
.PATH:	${.CURDIR}/../../usr.bin/rlogin
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-L${KRBOBJDIR} -lkrb -L${DESOBJDIR} -ldes
BINDIR=	/usr/bin
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
