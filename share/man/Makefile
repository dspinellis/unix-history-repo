#	@(#)Makefile	5.9 (Berkeley) 7/1/91

SUBDIR=	man1 man3 man4 man5 man7 man8

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 man0/COPYRIGHT \
	    ${DESTDIR}/usr/share/man/COPYRIGHT
	makewhatis /usr/share/man

makedb:
	makewhatis /usr/share/man

.include <bsd.subdir.mk>
