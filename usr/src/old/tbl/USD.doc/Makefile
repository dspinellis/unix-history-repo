#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/28.tbl
SRCS=	tbl
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
