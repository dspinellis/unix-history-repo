#	@(#)Makefile	5.3 (Berkeley) 5/11/90

PROG=	rcp
SRCS=	rcp.c krcmd.c kcmd.c
CFLAGS+=-DKERBEROS
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
