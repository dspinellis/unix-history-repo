#	@(#)Makefile	5.1 (Berkeley) 6/30/90

BINDIR=	/usr/share/doc/ps2
FILES=	00.contents Makefile Title
SUBDIR=	01.cacm 02.summary 03.uprog 04.implement 05.iosys 07.fp

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}

.include <bsd.subdir.mk>
