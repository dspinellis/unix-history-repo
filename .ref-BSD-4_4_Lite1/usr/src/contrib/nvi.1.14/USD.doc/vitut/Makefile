#	@(#)Makefile	8.2 (Berkeley) 4/22/94

DIR=	usd/11.vitut
SRCS=	vi.in vi.chars vi.summary vi.apwh.ms
MACROS=	-msU
CLEANFILES+=summary.* viapwh.*

paper.ps: ${SRCS} summary.ps viapwh.ps
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

summary.ps: vi.summary
	${TBL} vi.summary | ${ROFF}  > ${.TARGET}

viapwh.ps: vi.apwh.ms
	${ROFF} vi.apwh.ms > ${.TARGET}

.include <bsd.doc.mk>
