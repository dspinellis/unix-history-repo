#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/12.make
SRCS=	make.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
