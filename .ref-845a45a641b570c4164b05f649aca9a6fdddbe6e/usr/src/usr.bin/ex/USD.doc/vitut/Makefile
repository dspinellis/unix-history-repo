#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	usd/15.vi
SRCS=	vi.in vi.chars
MACROS=	-msU
CLEANFILES+=summary.* viapwh.*

paper.${PRINTER}: ${SRCS} summary.${PRINTER} viapwh.${PRINTER}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

summary.${PRINTER}: vi.summary
	${TBL} vi.summary | ${ROFF}  > ${.TARGET}

viapwh.${PRINTER}: vi.apwh.ms
	${ROFF} vi.viapwh > ${.TARGET}

.include <bsd.doc.mk>
