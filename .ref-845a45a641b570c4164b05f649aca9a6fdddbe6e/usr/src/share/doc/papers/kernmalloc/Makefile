#	@(#)Makefile	1.6 (Berkeley) %G%

DIR=	papers/kernmalloc
SRCS=	kernmalloc.t appendix.t
MACROS=	-ms
PRINTER=lz

paper.${PRINTER}: ${SRCS} alloc.fig usage.tbl
	${SOELIM} ${SRCS} | ${TBL} | ${PIC} | ${EQN} | ${GRIND} | ${ROFF} \
	> ${.TARGET}

.include <bsd.doc.mk>
