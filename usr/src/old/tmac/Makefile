#	@(#)Makefile	1.1 (Berkeley) %G%

# eliminating tmac.an.old to test tmac.an from /usr/src/.../groff/mancros/grot
# FILES=	tmac.an.old tmac.r tmac.andoc

FILES=	tmac.r tmac.andoc tmac.s
MSRCS=  doc doc-ditroff doc-common doc-nroff doc-syms

LINKS=	${BINDIR}/tmac/tmac.andoc ${BINDIR}/tmac/tmac.an
NOOBJ=	noobj

# old macros not currently installed...
# OFILES=	tmac.an.v6compat tmac.an6n tmac.an6t tmac.a tmac.cp

all clean cleandir depend lint tags:

beforeinstall:
	for i in ${MSRCS}; do \
		sed -f strip.sed < $$i > ${DESTDIR}${BINDIR}/tmac/tmac.$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}${BINDIR}/tmac/tmac.$$i; \
		chmod 444 ${DESTDIR}${BINDIR}/tmac/tmac.$$i; \
	done
	install -c -o ${BINOWN} -g ${BINOWN} -m 444 ${FILES} \
		${DESTDIR}${BINDIR}/tmac


.include <bsd.prog.mk>
