#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	usd/33.rogue
SRCS=	rogue.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
