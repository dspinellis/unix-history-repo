#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/18.sed
SRCS=	sed
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
