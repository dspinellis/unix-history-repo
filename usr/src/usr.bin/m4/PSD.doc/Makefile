#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/17.m4
SRCS=	m4.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
