#	@(#)Makefile	5.7 (Berkeley) %G%

SUBDIR=	man3 man3f man4 man5 man7 man8

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 makewhatis.sed \
	    ${DESTDIR}/usr/share/man/makewhatis.sed

makedb:
	for file in `find /usr/share/man -type f -name '*.0' -print`; do \
		sed -n -f /usr/share/man/makewhatis.sed $$file; \
	done | sort -u > whatis.db
	install -o ${BINOWN} -g ${BINGRP} -m 444 whatis.db \
	    ${DESTDIR}/usr/share/man

.include <bsd.subdir.mk>
