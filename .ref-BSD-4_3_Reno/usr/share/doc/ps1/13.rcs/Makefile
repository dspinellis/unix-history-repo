#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/13.rcs
SRCS=	rcs.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
