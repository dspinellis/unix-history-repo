#	@(#)Makefile	8.2 (Berkeley) %G%

# Missing: man3f

SUBDIR=	man1 man3 man4 man5 man7 man8

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 makewhatis.sed \
	    ${DESTDIR}${SEDF}

makedb:
	find /usr/share/man -type f -name '*.0' -print | \
	while read file; \
	do \
		sed -n -f ${SEDF} $$file; \
	done | col -b | sort -u > /tmp/whatis.db
	install -o ${BINOWN} -g ${BINGRP} -m 444 /tmp/whatis.db \
	    ${DESTDIR}/usr/share/man

.include <bsd.subdir.mk>
