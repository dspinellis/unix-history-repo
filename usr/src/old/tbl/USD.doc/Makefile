#	@(#)Makefile	5.2 (Berkeley) %G%

DIR=	usd/25.tbl
SRCS=	tbl
MACROS=	-msU

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
