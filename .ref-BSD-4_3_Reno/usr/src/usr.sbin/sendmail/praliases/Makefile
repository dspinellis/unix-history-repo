#	@(#)Makefile	5.1 (Berkeley) 5/11/90

PROG=	praliases
CFLAGS+=-I${.CURDIR}/../src
DPADD=	${LIBDBM}
LDADD=	-ldbm
NOMAN=	noman

.include "../../Makefile.inc"
.include <bsd.prog.mk>
