#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	pix
SRCDIR=	${.CURDIR}/../src
SRCS=	pix.c
OBJS+=	config.o
CFLAGS+=-I${SRCDIR}
CLEANFILES+=config.c

WHOAMI=	pi

.include "${SRCDIR}/config.include"
.include <bsd.prog.mk>
