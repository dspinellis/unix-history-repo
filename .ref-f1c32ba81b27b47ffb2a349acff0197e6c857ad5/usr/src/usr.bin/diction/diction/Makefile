#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	dprog
BINDIR=	/usr/libexec
MAN1=	diction.0
MLINKS=	diction.1 explain.1

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${.CURDIR}/dict.d ${DESTDIR}/usr/share/dict
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/diction.sh ${DESTDIR}/usr/bin/diction

.include "../../Makefile.inc"
.include <bsd.prog.mk>
