#	%W% (Berkeley) %G%

BINDIR=	/usr/bin

MAN1=	genclass.0

all genclass: ${MAN1}

clean depend lint tags:

cleandir:
	rm -f ${MAN1}

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/genclass.sh ${DESTDIR}${BINDIR}/genclass

.include <bsd.prog.mk>
