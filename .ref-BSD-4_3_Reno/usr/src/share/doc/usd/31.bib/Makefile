#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	usd/31.bib
SRCS=	bibdoc.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${BIB} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
