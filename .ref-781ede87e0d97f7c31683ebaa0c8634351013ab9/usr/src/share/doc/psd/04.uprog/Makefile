#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	ps2/03.uprog
SRCS=	p.mac p0 p1 p2 p3 p4 p5 p6 p8 p9
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${ROFF} ${MAC} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
