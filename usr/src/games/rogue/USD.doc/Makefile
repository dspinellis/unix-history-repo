#	@(#)Makefile	8.1 (Berkeley) %G%

DIR=	usd/33.rogue
SRCS=	rogue.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
