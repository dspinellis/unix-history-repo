#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/12.make
SRCS=	make.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
