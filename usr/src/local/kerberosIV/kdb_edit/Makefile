#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	kdb_edit
CFLAGS+=-DKERBEROS -DDEBUG -I. -I${.CURDIR}/../include
SRCS=	kdb_edit.c maketime.c
.PATH:	${.CURDIR}/../kdb_edit
DPADD=	${LIBKDB} ${LIBKRB} ${LIBDES}
LDADD=	-lkdb -lkrb -ldes
NOMAN=	noman

.include <bsd.prog.mk>
