#	@(#)Makefile	8.4 (Berkeley) %G%

DIR=	usd/12.exref
SRCS=	ex.rm
MACROS=	-ms
CLEANFILES=summary.*

paper.ps: ${SRCS} summary.ps
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

summary.ps: ex.summary
	${TBL} ex.summary | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
