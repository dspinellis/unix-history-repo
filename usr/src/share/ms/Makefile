#	@(#)Makefile	5.5 (Berkeley) 6/8/90

MSSRCS=	acc.ms cov.ms eqn.ms ref.ms tbl.ms ths.ms toc.ms
TMSRCS=	tmac.s tmac.os tmac.scover
NOOBJ=	noobj

# sources for old tmac.*, previously installed in /usr/lib/tmac
OSRCS=	tmac.sdisp tmac.skeep tmac.srefs

all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${MSSRCS} \
	    ${DESTDIR}${BINDIR}/ms
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${TMSRCS} \
	    ${DESTDIR}${BINDIR}/tmac

.include <bsd.prog.mk>
