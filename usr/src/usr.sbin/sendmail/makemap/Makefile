#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	makemap
CFLAGS+=-I${.CURDIR}/../src -DDBM_MAP -DBTREE_MAP -DHASH_MAP
DPADD=	${LIBDBM}
LDADD=	-ldbm
NOMAN=	noman

.include "../../Makefile.inc"
.include <bsd.prog.mk>
