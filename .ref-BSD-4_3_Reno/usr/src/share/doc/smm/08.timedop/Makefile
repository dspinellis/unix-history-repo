#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	smm/08.timedop
SRCS=	timed.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
