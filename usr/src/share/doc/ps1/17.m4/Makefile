#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/17.m4
SRCS=	m4.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
