#	@(#)Makefile	5.2 (Berkeley) 5/6/91

DATFILES=fortunes.dat startrek.dat zippy.dat fortunes-o.dat
CLEANFILES+=${DATFILES}

install: ${DATFILES}
	(cd ${.CURDIR} && install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${DATFILES:R} ${DESTDIR}/usr/share/games/fortune)
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${DATFILES} \
	    ${DESTDIR}/usr/share/games/fortune

fortunes-o.dat: ${.TARGET:R}
	${.CURDIR}/../strfile/obj/strfile -rsx \
	    ${.CURDIR}/${.TARGET:R} ${.TARGET}

fortunes.dat startrek.dat zippy.dat: ${.TARGET:R}
	${.CURDIR}/../strfile/obj/strfile -rs \
	    ${.CURDIR}/${.TARGET:R} ${.TARGET}

.include <bsd.prog.mk>
