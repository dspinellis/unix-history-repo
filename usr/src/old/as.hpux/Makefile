#	@(#)Makefile	5.2 (Berkeley) %G%

all hpux-as clean cleandir depend lint obj tags:

install:
	install -c -o ${BINOWN} -o ${BINGRP} -m ${BINMODE} hpux-as \
	    ${DESTDIR}${BINDIR}

.include <bsd.prog.mk>
