#	@(#)Makefile	5.1 (Berkeley) 6/29/90

PROG=	px_header
SRCDIR=	${.CURDIR}/../src
CFLAGS+=-I${SRCDIR}
LDFLAGS+=-N
SRCS=	px_header.c
OBJS+=	config.o
BINDIR=	/usr/libexec/pascal
NOMAN=	noman
CLEANFILES+=config.c

beforeinstall:
	strip px_header
	mv px_header temp
	dd if=temp of=px_header conv=sync
	rm -f temp

WHOAMI=		pi
.include "${SRCDIR}/config.include"
.include <bsd.prog.mk>
