#	@(#)Makefile	1.31 (Berkeley) %G%

SUBDIR=	lpass1 lpass2 llibs

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/lint.sh ${DESTDIR}/usr/old/bin/lint

.include <bsd.subdir.mk>
