#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	compact
SRCS=	compact.c tree.c
CFLAGS+=-I${.CURDIR}/../common_source
.PATH:	${.CURDIR}/../common_source
MLINKS=	compact.1 ccat.1 compact.1 uncompcat.1

.include "../../Makefile.inc"
.include <bsd.prog.mk>
