#	@(#)Makefile	5.4.1.1 (Berkeley) %G%

PROG=	rcp
SRCS=	rcp.c
#SRCS=	rcp.c krcmd.c kcmd.c
#CFLAGS+=-DKERBEROS -DCRYPT
#DPADD=	${LIBKRB} ${LIBDES}
#LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
