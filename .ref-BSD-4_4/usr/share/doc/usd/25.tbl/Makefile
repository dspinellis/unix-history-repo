#	@(#)Makefile	8.1 (Berkeley) 6/8/93

DIR=	usd/25.tbl
SRCS=	tbl
MACROS=	-msU

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
