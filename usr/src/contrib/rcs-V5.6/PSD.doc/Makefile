#	@(#)Makefile	8.2 (Berkeley) 5/24/94

DIR=	psd/13.rcs
SRCS=	rcs.ms
MACROS=	-ms

paper.ps: ${SRCS} ${DPADD}
	${SOELIM} ${SRCS} | ${PIC} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
