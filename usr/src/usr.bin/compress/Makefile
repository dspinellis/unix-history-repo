#	@(#)Makefile	8.1 (Berkeley) 6/6/93

PROG=	compress
LINKS=	${BINDIR}/compress ${BINDIR}/uncompress
MLINKS=	compress.1 uncompress.1 compress.1 zcat.1

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/zcat.sh ${DESTDIR}/usr/bin/zcat

.include <bsd.prog.mk>
