#	@(#)Makefile	5.4 (Berkeley) 10/21/90

PROG=	rcp
SRCS=	rcp.c krcmd.c kcmd.c
CFLAGS+=-DKERBEROS -DCRYPT
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
