#	@(#)Makefile	5.9 (Berkeley) 7/1/91

SUBDIR=	man1 man3 man4 man5 man7 man8

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 makewhatis.sed \
	    ${DESTDIR}/usr/share/man/makewhatis.sed
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 man0/COPYRIGHT \
	    ${DESTDIR}/usr/share/man/COPYRIGHT

makedb:
	for file in `find /usr/share/man/cat? -type f -print`; do \
		sed -n -f /usr/share/man/makewhatis.sed $$file; \
	done | sort -u > whatis.db
	install -o ${BINOWN} -g ${BINGRP} -m 444 whatis.db \
	    ${DESTDIR}/usr/share/man

.include <bsd.subdir.mk>
