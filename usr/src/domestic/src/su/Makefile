#	@(#)Makefile	5.5 (Berkeley) 5/11/90

PROG=	su
CFLAGS+=-DKERBEROS
DPADD=	${LIBKRB} ${LIBDES}
LDADD=	-lkrb -ldes
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
