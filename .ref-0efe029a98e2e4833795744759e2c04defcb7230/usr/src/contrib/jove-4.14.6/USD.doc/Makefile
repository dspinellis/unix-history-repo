#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	usd/17.jove
SRCS=	jove.1 jove.2 jove.3 jove.4
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
