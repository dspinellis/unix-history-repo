#	@(#)Makefile	5.2 (Berkeley) 6/30/90

DIR=	ps1/03.f77io
SRCS=	f77IO.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
