#	@(#)Makefile	5.3 (Berkeley) %G%

MAN1=	roffbib.0

all OTHERS: ${MAN}

clean cleandir depend lint tags:

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/indxbib.sh ${DESTDIR}${BINDIR}/indxbib
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/roffbib.sh ${DESTDIR}${BINDIR}/roffbib
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/tmac.bib ${DESTDIR}/usr/share/tmac
	(cd ${.CURDIR}/../papers; \
	    install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    INDEX Rbstjissue Rv7man ${DESTDIR}/usr/share/dict/papers)
	-/usr/libexec/mkey /usr/share/dict/papers/R* | (cd /var/db; \
	    /usr/libexec/inv -v -h997 -n Ind; chmod 444 Ind*)

.include "../../Makefile.inc"
.include <bsd.prog.mk>
