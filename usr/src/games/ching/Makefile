#	@(#)Makefile	5.2 (Berkeley) %G%

SUBDIR=	cno phx

all: ching.0

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/ching.sh ${DESTDIR}/usr/games/ching
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/hexagrams \
	    ${.CURDIR}/macros ${DESTDIR}/usr/share/games/ching

.include <bsd.prog.mk>
