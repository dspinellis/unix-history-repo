#	@(#)Makefile	5.1 (Berkeley) 6/30/90

DIR=	usd/32.diction
SRCS=	rm0 rm1 rm2 ap1 ap2
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
