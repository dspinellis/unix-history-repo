#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/19.awk
SRCS=	awk
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
