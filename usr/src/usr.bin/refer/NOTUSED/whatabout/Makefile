#	@(#)Makefile	5.2 (Berkeley) 6/24/90

PROG=	whatabout
CFLAGS+=-I${.CURDIR}/../refer
SRCS=	what1.c what2.c what3.c what4.c mkey3.c shell.c
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
NOMAN=	noman

.PATH:	${.CURDIR}/../hunt ${.CURDIR}/../mkey

.include "../../Makefile.inc"
.include <bsd.prog.mk>
