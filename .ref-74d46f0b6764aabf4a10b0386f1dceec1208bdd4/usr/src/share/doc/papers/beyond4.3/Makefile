#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	papers/beyond43
SRCS=	beyond43.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
