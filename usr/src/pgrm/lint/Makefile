#	@(#)Makefile	1.30 (Berkeley) 6/8/90

SUBDIR=	lpass1 lpass2 llibs

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/lint.sh ${DESTDIR}/usr/bin/lint

.include <bsd.subdir.mk>
