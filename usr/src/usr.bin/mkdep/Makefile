#	@(#)Makefile	5.2 (Berkeley) %G%

MAN=	mkdep.0

all mkdep: ${MAN}

clean depend lint tags:

cleandir:
	rm -f ${MAN}

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/mkdep.sh ${DESTDIR}/usr/bin/mkdep
	install -c -o ${MANOWN} -g ${MANGRP} -m ${MANMODE} ${MAN} \
	    ${DESTDIR}${MANDIR}1

.include <bsd.prog.mk>
