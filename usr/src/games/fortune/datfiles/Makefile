#	@(#)Makefile	5.3 (Berkeley) %G%

# If TYPE is real, the real obscene fortunes get installed.  If TYPE is
# fake, they don't.  See how easy that was?

TYPE=	real
#TYPE=	fake

SRCS=	fortunes startrek zippy
BLDS=	fortunes.dat startrek.dat zippy.dat fortunes-o fortunes-o.dat
CLEANFILES+=${BLDS}

install: ${SRCS} ${BLDS}
	(cd ${.CURDIR} && install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${SRCS} ${DESTDIR}/usr/share/games/fortune)
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${BLDS} \
	    ${DESTDIR}/usr/share/games/fortune

fortunes.dat startrek.dat zippy.dat:
	${.CURDIR}/../strfile/obj/strfile -rs ${.CURDIR}/${.TARGET:R} ${.TARGET}

fortunes-o.dat: fortunes-o
	${.CURDIR}/../strfile/obj/strfile -rsx fortunes-o ${.TARGET}

fortunes-o: fortunes-o.${TYPE}
	caesar 13 < ${.CURDIR}/${.ALLSRC} > ${.TARGET}

.include <bsd.prog.mk>
