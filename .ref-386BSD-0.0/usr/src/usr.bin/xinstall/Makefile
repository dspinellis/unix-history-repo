#	@(#)Makefile	5.5 (Berkeley) 6/24/90

PROG=	xinstall
MAN1=	install.0
CLEANFILES=xinstall

install: maninstall
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROG} ${DESTDIR}${BINDIR}/install

.include <bsd.prog.mk>


