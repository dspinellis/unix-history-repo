#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	usd/21.msdiffs
SRCS=	ms.diffs
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
