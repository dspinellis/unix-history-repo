#	@(#)Makefile	1.4 (Berkeley) %G%

DIR=	papers/kernmalloc
SRCS=	kernmalloc.t appendix.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS} alloc.fig usage.tbl
	${SOELIM} ${SRCS} | ${TBL} | ${PIC} | ${EQN} | ${GRIND} | ${ROFF}

.include <bsd.doc.mk>
