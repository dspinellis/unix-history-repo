#	@(#)Makefile	5.1 (Berkeley) 6/30/90

DIR=	usd/28.tbl
SRCS=	tbl
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
