#	@(#)Makefile	6.12 (Berkeley) %G%

FILES=	tmac.an.old tmac.r tmac.doc tmac.andoc tmac.doc.old tmac.doc-ditroff \
	tmac.doc-nroff tmac.doc-common tmac.doc-syms
MSRCS=  doc-parse doc-ditroff doc-common doc-nroff doc-syms
LINKS=	${BINDIR}/tmac/tmac.andoc ${BINDIR}/tmac/tmac.an
NOOBJ=	noobj

# old macros not currently installed...
OFILES=	tmac.an.v6compat tmac.an6n tmac.an6t tmac.a tmac.cp

all clean cleandir depend lint tags:

beforeinstall:
	for i in ${MSRCS}; do \
		sed -f strip.sed < $$i > tmac.$$i; \
	done
	install -c -o ${BINOWN} -g ${BINOWN} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/tmac

.include <bsd.prog.mk>
