#	Makefile	1.2	%G%
#

PROG=	tac
SCRIPT=	tmail.sh
MAN1=	tac.0 tmail.0

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/tmail.sh ${DESTDIR}${BINDIR}/tmail

.include <bsd.prog.mk>
