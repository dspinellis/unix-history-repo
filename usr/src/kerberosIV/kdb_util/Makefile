#	@(#)Makefile	5.2 (Berkeley) 2/14/91

PROG=	kdb_util
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../kdb_edit \
	-I${.CURDIR}/../include
SRCS=	kdb_util.c maketime.c
.PATH:	${.CURDIR}/../kdb_edit
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
NOMAN=	noman

.include <bsd.prog.mk>
