#	@(#)Makefile	5.1 (Berkeley) 6/8/90

all hpux-as clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -o ${BINGRP} -m ${BINMODE} hpux-as \
	    ${DESTDIR}${BINDIR}

.include <bsd.prog.mk>
