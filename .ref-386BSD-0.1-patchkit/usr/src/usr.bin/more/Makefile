#	@(#)Makefile	5.11 (Berkeley) 6/25/90

PROG=	more
DPADD=	${LIBTERM}
LDADD=	-ltermcap
MLINKS=	more.1 page.1
LINKS=	${BINDIR}/more ${BINDIR}/page

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/more.help \
	    ${DESTDIR}/usr/share/misc/omore.help

.include <bsd.prog.mk>
