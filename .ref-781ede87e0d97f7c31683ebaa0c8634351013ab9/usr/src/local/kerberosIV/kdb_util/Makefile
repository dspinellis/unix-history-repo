#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	kdb_util
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../kdb_edit \
	-I${.CURDIR}/../include
SRCS=	kdb_util.o maketime.o
.PATH:	${.CURDIR}/../kdb_edit
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
NOMAN=	noman

.include <bsd.prog.mk>
