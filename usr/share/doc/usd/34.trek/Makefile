#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	usd/34.trek
SRCS=	trek.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
