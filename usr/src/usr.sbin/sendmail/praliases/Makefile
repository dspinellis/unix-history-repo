#	@(#)Makefile	6.1 (Berkeley) %G%

PROG=	praliases
CFLAGS+=-I${.CURDIR}/../src
DPADD=	${LIBDBM}
LDADD=	-ldbm
NOMAN=	noman

.include "../../Makefile.inc"
.include <bsd.prog.mk>
