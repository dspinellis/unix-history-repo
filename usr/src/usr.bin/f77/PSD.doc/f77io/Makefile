#	@(#)Makefile	5.2 (Berkeley) %G%

DIR=	ps1/03.f77io
SRCS=	f77IO.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
