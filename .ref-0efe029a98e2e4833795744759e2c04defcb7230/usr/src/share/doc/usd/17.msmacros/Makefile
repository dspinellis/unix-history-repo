#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/20.msmacros
SRCS=	ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
