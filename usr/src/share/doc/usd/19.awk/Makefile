#	@(#)Makefile	5.1 (Berkeley) 6/30/90

DIR=	usd/19.awk
SRCS=	awk
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
