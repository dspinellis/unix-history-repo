#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	makemap
CFLAGS+=-I${.CURDIR}/../src -DBTREE_MAP -DDBM_MAP
DPADD=	${LIBDBM}
LDADD=	-ldbm
NOMAN=	noman

.include "../../Makefile.inc"
.include <bsd.prog.mk>
