#	@(#)Makefile	8.2 (Berkeley) 4/22/94

DIR=	usd/12.exref
SRCS=	ex.rm ex.summary
MACROS=	-msU
CLEANFILES=summary.*

paper.ps: ${SRCS} summary.ps
	${ROFF} ${SRCS} > ${.TARGET}

summary.ps: ex.summary
	${TBL} ex.summary | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
