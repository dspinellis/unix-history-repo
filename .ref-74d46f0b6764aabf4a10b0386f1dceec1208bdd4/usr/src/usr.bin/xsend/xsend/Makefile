#	@(#)Makefile	4.8 (Berkeley) %G%

PROG=	xsend
CFLAGS+=-I${.CURDIR}/../common_source
SRCS=	xsend.c lib.c
DPADD=	${LIBMP} ${LIBCOMPAT}
LDADD=	-lmp -lcompat
MAN1=	xsend.0
MLINKS=	xsend.1 enroll.1 xsend.1 xget.1
.PATH:	${.CURDIR}/../common_source

.include "../../Makefile.inc"
.include <bsd.prog.mk>
