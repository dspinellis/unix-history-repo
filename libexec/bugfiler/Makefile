#	@(#)Makefile	5.17 (Berkeley) 5/24/90
# Fixed to use freefall.cdrom.com as a bugs host - rgrimes 93/08/03

PROG=	bugfiler
CFLAGS+=-I${.CURDIR} -DBUGS_HOME=\"owner-FreeBSD-bugs@freefall.cdrom.com\"
SRCS=	bugfiler.c error.c gethead.c process.c redist.c reply.c
BINOWN=	root
BINMODE=4555
MAN1=	sendbug.1
MAN8=	bugfiler.8

beforeinstall:
	install -c -o bin -g ${BINGRP} -m 555 \
	    ${.CURDIR}/sendbug.sh ${DESTDIR}/usr/bin/sendbug
	install -c -o bin -g ${BINGRP} -m 444 ${.CURDIR}/bugformat \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
