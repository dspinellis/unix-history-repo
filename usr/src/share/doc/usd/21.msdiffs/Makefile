#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	usd/21.msdiffs
SRCS=	ms.diffs
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
