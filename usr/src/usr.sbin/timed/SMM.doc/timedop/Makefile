#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	smm/08.timedop
SRCS=	timed.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
