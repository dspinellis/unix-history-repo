#	@(#)Makefile	5.6 (Berkeley) %G%

MAN1=	machine.0

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/machine.sh ${DESTDIR}${BINDIR}/machine

.include <bsd.prog.mk>
