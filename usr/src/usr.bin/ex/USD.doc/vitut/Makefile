#	@(#)Makefile	8.4 (Berkeley) %G%

DIR=	usd/11.vitut
SRCS=	vi.in vi.chars
MACROS=	-ms
CLEANFILES+=summary.* viapwh.*

paper.ps: ${SRCS} summary.ps viapwh.ps
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

summary.ps: vi.summary
	${TBL} vi.summary | ${ROFF}  > ${.TARGET}

viapwh.ps: vi.apwh.ms
	${TBL} vi.apwh.ms | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
