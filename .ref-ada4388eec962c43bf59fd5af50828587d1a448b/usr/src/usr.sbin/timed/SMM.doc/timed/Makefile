#	@(#)Makefile	8.2 (Berkeley) %G%

DIR=	smm/12.timed
SRCS=	timed.ms
MACROS=	-ms

paper.ps: ${SRCS}
	${SOELIM} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
