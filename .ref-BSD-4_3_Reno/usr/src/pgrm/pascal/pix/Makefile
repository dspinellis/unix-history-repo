#	@(#)Makefile	5.1 (Berkeley) 6/29/90

PROG=	pix
SRCDIR=	${.CURDIR}/../src
SRCS=	pix.c
OBJS+=	config.o
CFLAGS+=-I${SRCDIR}
CLEANFILES+=config.c

WHOAMI=	pi

.include "${SRCDIR}/config.include"
.include <bsd.prog.mk>
