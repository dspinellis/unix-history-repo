#	@(#)bsd.doc.mk	5.2 (Berkeley) 6/30/90

PRINTER=lz

BIB?=		bib
EQN?=		/usr/local/deqn -P${PRINTER}
GREMLIN?=	grn -P${PRINTER}
GRIND?=		vgrind -f
INDXBIB?=	indxbib
PIC?=		pic -P${PRINTER}
REFER?=		refer
ROFF?=		psroff -t ${MACROS} ${PAGES} -P${PRINTER}
SOELIM?=	soelim
TBL?=		dtbl -P${PRINTER}

.PATH: ${.CURDIR}

.if !target(print)
print: paper.${PRINTER}
	lpr -P${PRINTER} paper.${PRINTER}
.endif

clean cleandir:
	rm -f paper.* [eE]rrs mklog ${CLEANFILES}

FILES?=	${SRCS}
install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    Makefile ${FILES} ${EXTRA} ${DESTDIR}${BINDIR}/${DIR}

spell: ${SRCS}
	spell ${SRCS} | sort | comm -23 - spell.ok > paper.spell

BINDIR?=	/usr/share/doc
BINGRP?=	bin
BINOWN?=	bin
BINMODE?=	444
