#	@(#)Makefile	5.4 (Berkeley) %G%

PROG=	xinstall
MAN1=	install.0

install: maninstall
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROG} ${DESTDIR}${BINDIR}/install

.include <bsd.prog.mk>


