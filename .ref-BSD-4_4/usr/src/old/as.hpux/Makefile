#	@(#)Makefile	5.3 (Berkeley) 7/17/92

all hpux-as clean cleandir depend lint obj tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} hpux-as \
	    ${DESTDIR}${BINDIR}

.include <bsd.prog.mk>
