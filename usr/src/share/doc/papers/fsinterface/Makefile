#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	papers/fsinterface
SRCS=	abstract.ms fsinterface.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
