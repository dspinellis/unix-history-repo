#	@(#)Makefile	5.1 (Berkeley) 5/11/90

MAN1=	style.0

all style: ${MAN1}

clean depend lint tags:

cleandir:
	rm -f ${MAN1}

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	     ${.CURDIR}/style.sh ${DESTDIR}${BINDIR}/style


.include "../../Makefile.inc"
.include <bsd.prog.mk>
