#	@(#)Makefile	8.1 (Berkeley) 8/14/93

DIR=	psd/13.rcs
SRCS=	rcs.ms
MACROS=	-msU

paper.ps: ${SRCS} ${DPADD}
	${SOELIM} ${SRCS} | ${PIC} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
