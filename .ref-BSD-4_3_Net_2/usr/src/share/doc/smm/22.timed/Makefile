#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	smm/22.timed
SRCS=	timed.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${SOELIM} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
