#	@(#)Makefile	5.3 (Berkeley) %G%

MAN1=	mkdep.0

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/mkdep.sh ${DESTDIR}/usr/bin/mkdep

.include <bsd.prog.mk>
