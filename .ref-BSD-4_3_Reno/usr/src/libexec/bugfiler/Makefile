#	@(#)Makefile	5.17 (Berkeley) 5/24/90

PROG=	bugfiler
CFLAGS+=-I${.CURDIR}
SRCS=	bugfiler.c error.c gethead.c process.c redist.c reply.c
BINOWN=	root
BINMODE=4555
MAN1=	sendbug.0
MAN8=	bugfiler.0

beforeinstall:
	install -c -o bin -g ${BINGRP} -m 555 \
	    ${.CURDIR}/sendbug.sh ${DESTDIR}/usr/bin/sendbug
	install -c -o bin -g ${BINGRP} -m 444 ${.CURDIR}/bugformat \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
