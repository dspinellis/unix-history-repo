#	@(#)Makefile	8.1 (Berkeley) %G%

# Missing: man3f

SUBDIR=	man1 man3 man4 man5 man7 man8

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 makewhatis.sed \
	    ${DESTDIR}/usr/share/man/makewhatis.sed

makedb:
	for file in `find /usr/share/man -type f -name '*.0' -print`; do \
		sed -n -f /usr/share/man/makewhatis.sed $$file; \
	done | col -b | sort -u > /tmp/whatis.db
	install -o ${BINOWN} -g ${BINGRP} -m 444 /tmp/whatis.db \
	    ${DESTDIR}/usr/share/man

.include <bsd.subdir.mk>
