
BINDIR=	/usr/share/doc

MANGRP?=	bin
MANOWN?=	bin
MANMODE?=	444

PRINTER=lz
ROFF=	ditroff -P${PRINTER}
TBL=	dtbl -P${PRINTER} ${PAGES}

print:
.if defined(DOTBL)
	${TBL} |
	${ROFF} ${MACROS} -t ${SRCS} | psdit > paper.${PRINTER}
.else
	${ROFF} ${MACROS} -t ${SRCS} | psdit > paper.${PRINTER}
.endif

install:
	install -c -o ${MANOWN} -g ${MANGRP} -m ${MANMODE} \
	    Makefile ${SRCS} ${EXTRA} ${DESTDIR}${BINDIR}/${DIR}
