#	@(#)Makefile	5.1 (Berkeley) %G%

MAN=	roffbib.0

all OTHERS: ${MAN}

clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/indxbib.sh ${DESTDIR}${BINDIR}/indxbib
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/roffbib.sh ${DESTDIR}${BINDIR}/roffbib
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/tmac.bib ${DESTDIR}/usr/share/tmac
	sh ${.CURDIR}/runinv ${DESTDIR}/usr/share/dict/papers

.include "../../Makefile.inc"
.include <bsd.prog.mk>
