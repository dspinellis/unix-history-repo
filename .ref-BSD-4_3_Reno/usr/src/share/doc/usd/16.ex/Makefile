#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	usd/16.ex
SRCS=	ex.rm
MACROS=	-msU
CLEANFILES=summary.*

paper.${PRINTER}: ${SRCS} summary.${PRINTER}
	${ROFF} ${SRCS} > ${.TARGET}

summary.${PRINTER}: ex.summary
	${TBL} ex.summary | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
