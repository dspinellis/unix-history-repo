#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	xget
CFLAGS+=-I${.CURDIR}/../common_source
SRCS=	xget.c lib.c
NOMAN=	man
DPADD=	${LIBMP} ${LIBCOMPAT}
LDADD=	-lmp -lcompat
.PATH:	${.CURDIR}/../common_source

.include "../../Makefile.inc"
.include <bsd.prog.mk>
