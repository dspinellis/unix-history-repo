#	@(#)Makefile	6.4 (Berkeley) 6/30/90

DIR=	usd/22.memacros
SRCS=	intro.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
