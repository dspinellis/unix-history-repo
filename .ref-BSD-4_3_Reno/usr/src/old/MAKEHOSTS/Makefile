#	@(#)Makefile	5.2 (Berkeley) 6/19/90

NOOBJ=	noobj

MAKEHOSTS all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    MAKEHOSTS.sh ${DESTDIR}${BINDIR}/MAKEHOSTS

.include <bsd.prog.mk>
