#	@(#)Makefile	5.2 (Berkeley) %G%

DIR=	papers/fsinterface
SRCS=	fsinterface.ms
MACROS=	-ms
PRINTER=lz

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
