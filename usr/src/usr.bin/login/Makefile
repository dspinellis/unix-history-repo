#	@(#)Makefile	5.5 (Berkeley) %G%

CFLAGS+=-DKERBEROS
PROG=	login
DPADD=	${LIBUTIL} ${LIBKRB} ${LIBDES}
LDADD=	-lutil -lkrb -ldes
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
